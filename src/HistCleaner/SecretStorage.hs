{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module HistCleaner.SecretStorage where

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Crypto.Error (CryptoFailable(..))
import Data.Base64.Types (extractBase64)
import Data.ByteString (ByteString)
import Data.ByteString.Base64
import qualified Data.ByteString.Char8 as C8
import System.Directory (doesDirectoryExist, doesFileExist, removeFile)
import System.Posix (ownerModes)
import System.Posix.Directory (createDirectory)

import HistCleaner.Config
import qualified HistCleaner.Hash as Hash

data SStorageResult
  = SSuccess
  | SHashFail
  | StoreFail String
  | NoMatch
  | RemoveFail String
  deriving (Show)

storeSecret :: ByteString -> Bool -> IO SStorageResult
storeSecret secret keepStoredEndlines = do
  vault <- getSecrets
  case Hash.hash (salt vault) secret of
    CryptoFailed _ -> pure SHashFail
    CryptoPassed res ->
      if res `elem` secrets vault
        then do
          pure $ StoreFail "Secret already stored."
        else do
          storeLine res
          unless keepStoredEndlines $ getEndInfosPath >>= doesFileExist >>= \fileExists -> when fileExists $ removeFile =<< getEndInfosPath
          pure SSuccess

-- Storing config & hashes
storeLine :: ByteString -> IO ()
storeLine str = do
  filepath <- getSecretsFilepath
  C8.appendFile filepath $ extractBase64 (encodeBase64' str) <> "\n"

removeSecret :: ByteString -> IO SStorageResult
removeSecret secret = do
  vault <- getSecrets
  case Hash.hash (salt vault) secret of
    CryptoFailed _ -> pure SHashFail
    CryptoPassed res -> do
      removeLine res

removeLine :: ByteString -> IO SStorageResult
removeLine str = do
  filepath <- getSecretsFilepath
  contents <- C8.readFile filepath
  let string = extractBase64 $ encodeBase64' str
      strings = C8.lines contents
      res = filter (string /=) strings
  if length res == length strings
    then do
      pure $ RemoveFail "Secret not found in vault."
    else do
      C8.writeFile filepath $ C8.unlines res
      pure SSuccess

data Vault =
  Vault
    { salt :: !ByteString
    , secrets :: ![ByteString]
    }
  deriving (Show)

getSecrets :: IO Vault
getSecrets = do
  configFolder <- getConfigFolder
  exists <- doesDirectoryExist configFolder
  unless exists $ createDirectory configFolder ownerModes
  filepath <- getSecretsFilepath
  catch
    (do contents <- C8.readFile filepath
        case C8.lines contents of
          (encSalt:_:encRest) ->
            case decodeBase64Untyped encSalt of
              Left _ -> error "Decoding error"
              -- Enforce strictness to not store undecodced and decoded values in memory
              Right salt -> pure $! Vault salt $!! decodeSecrets encRest
          _ -> error "Decoding error")
    (\(e :: IOException) -> do
       salt <- Hash.newSalt
       C8.writeFile filepath $
         extractBase64 (encodeBase64' salt) <>
         "\n----------------------------------\n"
       pure $ Vault salt [])

-- Inverts the list while decoding it
decodeSecrets :: [ByteString] -> [ByteString]
decodeSecrets input =
  case input of
    [] -> []
    (first:rest) ->
      case decodeBase64Untyped first of
        Left _ -> do
          error "Decoding error"
        Right b -> (b : decodeSecrets rest)
