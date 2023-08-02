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
import System.Directory
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

cleanFile :: Bool -> FilePath -> IO CleanResult
cleanFile force filepath = do
  vault <- St.getSecrets
  content <- C8.readFile filepath --TODO: Don't read twice? Use vault?
  prevELinesInfo <- getPrevEndLines filepath
  let contLines = C8.lines content
      fileType = getFileType filepath
      -- Skip already checked lines if force is false and endLines exist
      reducedLines =
        if' force contLines $
        dropAlreadyChecked (endLines prevELinesInfo) contLines
      -- Seperate already checked lines to reinsert later
      alreadyCheckedLines =
        if' force [] $ take (length contLines - length reducedLines) contLines
  if null reducedLines
    then do
      putStrLn $ "All lines of " <> filepath <> " have already been checked."
      pure CSuccess
    else do
      let func =
            cleanText fileType reducedLines (St.salt vault) (St.secrets vault)
          (resLines, rCode) = runState func CSuccess
          tempFilepath = "." <> filepath <> ".tmp"
      file <- openFile tempFilepath WriteMode
      _ <- installHandler keyboardSignal (Catch $ hFlush file) Nothing
      C8.hPutStr file $ C8.unlines (alreadyCheckedLines ++ resLines)
      hClose file
      copyFile tempFilepath filepath
      removeFile tempFilepath
      if rCode == CSuccess
        then do
          storeEndlines filepath prevELinesInfo resLines
          pure CSuccess
        else do
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

data EndLinesInfo =
  EndLinesInfo
    { filePath :: ByteString
    , encodedLines :: ByteString
    , endLines :: [ByteString]
    }
  deriving (Show, Eq)

emptyEndLinesInfo :: EndLinesInfo
emptyEndLinesInfo = EndLinesInfo "" "" []

getPrevEndLines :: FilePath -> IO EndLinesInfo
getPrevEndLines filepath = do
  endInfosPath <- getEndInfosPath
  exists <- fileExist endInfosPath
  if not exists
    then pure emptyEndLinesInfo
    else do
      content <- C8.readFile endInfosPath
      let contLines = C8.lines content
      if odd $ length contLines
        then do
          C8.writeFile endInfosPath ""
          pure emptyEndLinesInfo
        else do
          let endLineInfos = map (\[x, y] -> (x, y)) $ chunksOf 2 contLines
              filePath = fromString filepath
          case lookup filePath endLineInfos of
            Just encodedLines ->
              case decodeBase64 encodedLines of
                Left _ -> pure emptyEndLinesInfo
                Right endLines -> do
                  pure $ EndLinesInfo filePath encodedLines $ C8.lines endLines
            Nothing -> pure emptyEndLinesInfo

storeEndlines :: FilePath -> EndLinesInfo -> [ByteString] -> IO ()
storeEndlines filepath prevELinesInfo text = do
  endInfosPath <- getEndInfosPath
  let curEndLines = encodeBase64' . C8.unlines $ lastN' 3 text
      curEndInfo = [fromString filepath, curEndLines]
  if prevELinesInfo == emptyEndLinesInfo
    then do
      C8.appendFile endInfosPath $ C8.unlines curEndInfo
    else do
      content <- C8.readFile endInfosPath
      let contLines = C8.lines content
      -- Delete previous endLineInfo
          filteredLines =
            filter
              (\x ->
                 x /= filePath prevELinesInfo &&
                 x /= encodedLines prevELinesInfo)
              contLines
      C8.writeFile endInfosPath $ C8.unlines $ filteredLines ++ curEndInfo

getEndInfosPath :: IO FilePath
getEndInfosPath = do
  configFolderPath <- getConfigFolder
  pure $ configFolderPath </> "endInfos"

dropAlreadyChecked :: [ByteString] -> [ByteString] -> [ByteString]
dropAlreadyChecked endLines allLines =
  if length allLines < 3 || length endLines < 3
    then allLines
    else let reducedLines = dropWhile (/= head endLines) allLines
          in if reducedLines !! 1 == endLines !! 1 &&
                reducedLines !! 2 == endLines !! 2
               then drop 3 reducedLines
               else dropAlreadyChecked endLines $ tail reducedLines

lastN' :: Int -> [a] -> [a]
lastN' n xs = foldl' (const . drop 1) xs (drop n xs)

if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y
