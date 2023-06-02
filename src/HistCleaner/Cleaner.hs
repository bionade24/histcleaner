{-# language OverloadedStrings #-}

module HistCleaner.Cleaner where

import Control.Monad.State.Lazy
import Crypto.Error (CryptoFailable(..))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.UTF8 (toString)
import System.Posix.Signals
import System.IO

import HistCleaner.Hash
import qualified HistCleaner.SecretStorage as St


data CleanResult
  = CSuccess
  | CHashFail String
  | ReadFail FilePath
  | WriteFail FilePath
  deriving (Eq, Show)

{-TODO:
 - support ~/.lesshst Syntax "eintrag
 - other special cases
 - Remember to which point a histfile has already been cleaned and resume from there
-}

cleanFile :: FilePath -> IO CleanResult
cleanFile filepath = do
  vault <- St.getSecrets
  -- Get content of file to be cleaned
  content <- C8.readFile filepath
  let
    func = cleanText (C8.lines content) (St.salt vault) (St.secrets vault)
    (resCon, rCode) = runState func CSuccess
  file <- openFile filepath WriteMode
  _ <- installHandler keyboardSignal (Catch $ hFlush file) Nothing
  C8.hPutStr file $ C8.unlines resCon
  hClose file
  pure rCode

cleanText :: [ByteString] -> ByteString -> [ByteString] -> State CleanResult [ByteString]
cleanText text salt secrets = do
  case text of
    [] -> do
      put CSuccess
      return []

    (line : rest) ->
      let
        (resLine, rCode) = runState (cleanLine (C8.words line) salt secrets) CSuccess
      in
        if rCode == CSuccess then do
          res <- cleanText rest salt secrets
          return ( C8.unwords resLine : res )
        else do
          put rCode
          pure []

cleanLine :: [ByteString] -> ByteString -> [ByteString] -> State CleanResult [ByteString]
cleanLine line salt secrets = do
  case line of
    [] -> do
      put CSuccess
      return []

    (word : rest) ->
      case hash salt word of
        CryptoPassed hres -> do
          put CSuccess
          res <- cleanLine rest salt secrets
          if hres `elem` secrets
            then do
              return ( "redacted" : res )
            else do
              return ( word : res )

        CryptoFailed _ -> do
          put $ CHashFail $ toString word
          return []
