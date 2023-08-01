{-# LANGUAGE OverloadedStrings #-}

module HistCleaner.Cleaner where

import Control.Monad.State.Lazy
import Crypto.Error (CryptoFailable(..))
import Data.ByteString (ByteString)
import Data.ByteString.Base64
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.UTF8 (fromString, toString)
import Data.Foldable
import Data.List.Split
import Data.Maybe
import System.FilePath
import System.IO
import System.Posix.Files
import System.Posix.Signals

import HistCleaner.FileParser
import HistCleaner.Hash
import qualified HistCleaner.SecretStorage as St
import HistCleaner.SecretStorage (getConfigFolder)

data CleanResult
  = CSuccess
  | CHashFail String
  | ReadFail FilePath
  | WriteFail FilePath
  deriving (Eq, Show)

cleanFile :: FilePath -> IO CleanResult
cleanFile filepath = do
  vault <- St.getSecrets
  content <- C8.readFile filepath --TODO: Don't read twice? Use vault?
  endLines <- getEndlines filepath
  let contLines = C8.lines content
      fileType = getFileType filepath
      reducedLines =
        fromMaybe contLines $
        dropAlreadyChecked (fromMaybe [] endLines) $ C8.lines content
      -- Seperate already checked Lines to reinsert later
      alreadyCheckedLines =
        take (length contLines - length reducedLines) contLines
    {- TODO:
     - 4. Provide --force option
     -}
      func = cleanText fileType reducedLines (St.salt vault) (St.secrets vault)
      (resLines, rCode) = runState func CSuccess
  file <- openFile filepath WriteMode
  _ <- installHandler keyboardSignal (Catch $ hFlush file) Nothing
  C8.hPutStr file $ C8.unlines (alreadyCheckedLines ++ resLines)
  hClose file
  if rCode == CSuccess
    then do
      storeEndlines filepath resLines --TODO: Maybe do split here for memory usage ?
      pure ()
    else do
      pure ()
  pure rCode

cleanText ::
     FileType
  -> [ByteString]
  -> ByteString
  -> [ByteString]
  -> State CleanResult [ByteString]
cleanText fileType text salt secrets = do
  case text of
    [] -> do
      put CSuccess
      return []
    (rawLine:rest) ->
      case parseLine fileType rawLine of
        Skip -> do
          res <- cleanText fileType rest salt secrets
          return (rawLine : res)
        Simple line ->
          let (resLine, rCode) = runState (cleanLine line salt secrets) CSuccess
           in if rCode == CSuccess
                then do
                  res <- cleanText fileType rest salt secrets
                  return (reFormatLine fileType resLine : res)
                else do
                  put rCode
                  return []
        Special formStr maySec ->
          error "Parsing of special lines currently unsupported"

cleanLine ::
     [ByteString]
  -> ByteString
  -> [ByteString]
  -> State CleanResult [ByteString]
cleanLine line salt secrets = do
  case line of
    [] -> do
      put CSuccess
      return []
    (word:rest) ->
      case hash salt word of
        CryptoPassed hres -> do
          put CSuccess
          res <- cleanLine rest salt secrets
          if hres `elem` secrets
            then do
              return ("redacted" : res)
            else do
              return (word : res)
        CryptoFailed _ -> do
          put $ CHashFail $ toString word
          return []

getEndlines :: FilePath -> IO (Maybe [ByteString])
getEndlines filepath = do
  endMarkersPath <- getEndMarkersPath
  exists <- fileExist endMarkersPath
  if not exists
    then pure Nothing
    else do
      content <- C8.readFile endMarkersPath
      let contLines = C8.lines content
      if odd $ length contLines
        then do
          C8.writeFile endMarkersPath ""
          pure Nothing
        else do
          let endMarkers = map (\[x, y] -> (x, y)) $ chunksOf 2 contLines
          case lookup (fromString filepath) endMarkers of
            Just encLines ->
              case decodeBase64 encLines of
                Left _ -> pure Nothing
                Right endLines -> do
                  let restLines =
                        filter
                          (\x -> x /= fromString filepath && x /= encLines)
                          contLines
                  C8.writeFile endMarkersPath $ C8.unlines restLines
                  pure $ Just $ C8.lines endLines
            Nothing -> pure Nothing

-- Parse this thing into a structure
storeEndlines :: FilePath -> [ByteString] -> IO ()
storeEndlines filepath content = do
  let endLines = encodeBase64' . C8.unlines $ lastN' 3 content
      endMarker = [fromString filepath, endLines]
  endMarkersPath <- getEndMarkersPath
  C8.appendFile endMarkersPath $ C8.unlines endMarker
  pure ()

getEndMarkersPath :: IO FilePath
getEndMarkersPath = do
  configFolderPath <- getConfigFolder
  pure $ configFolderPath </> "endMarkers"

dropAlreadyChecked :: [ByteString] -> [ByteString] -> Maybe [ByteString]
dropAlreadyChecked endLines allLines =
  if length endLines < 3
    then Just allLines
    else let reducedLines = dropWhile (/= head endLines) allLines
          in if null reducedLines
               then Nothing
               else if reducedLines !! 1 == endLines !! 1 &&
                       reducedLines !! 2 == endLines !! 2
                      then Just $ drop 3 reducedLines
                      else dropAlreadyChecked reducedLines allLines

--TODO: Mem consumption test
lastN' :: Int -> [a] -> [a]
lastN' n xs = foldl' (const . drop 1) xs (drop n xs)
