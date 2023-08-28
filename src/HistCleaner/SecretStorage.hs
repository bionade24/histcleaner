{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module HistCleaner.SecretStorage where

import Control.Exception
import Crypto.Error (CryptoFailable(..))
import Data.ByteString (ByteString)
import Data.ByteString.Base64
import qualified Data.ByteString.Char8 as C8
import System.Directory (doesDirectoryExist)
import System.Environment.XDG.BaseDir
import System.Posix.Directory (createDirectory)

import qualified HistCleaner.Hash as Hash
import System.Posix (ownerModes)

data SStorageResult
  = SSuccess
  | SHashFail
  | StoreFail String
  | NoMatch
  | RemoveFail String
  deriving (Show)

storeSecret :: ByteString -> IO SStorageResult
storeSecret secret = do
  vault <- getSecrets
  case Hash.hash (salt vault) secret of
    CryptoFailed _ -> pure SHashFail
    CryptoPassed res ->
      if res `elem` secrets vault
        then do
          pure $ StoreFail "Secret already stored."
        else do
          storeLine res
          pure SSuccess

-- Storing config & hashes
storeLine :: ByteString -> IO ()
storeLine str = do
  filepath <- getSecretsFilepath
  C8.appendFile filepath $ encodeBase64' str <> "\n"

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
  let string = encodeBase64' str
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
    { salt :: ByteString
    , secrets :: [ByteString]
    }
  deriving (Show)

getSecrets :: IO Vault
getSecrets = do
  configFolder <- getConfigFolder
  exists <- doesDirectoryExist configFolder
  if not exists
    then do
      createDirectory configFolder ownerModes
      pure ()
    else pure ()
  filepath <- getSecretsFilepath
  catch
    (do contents <- C8.readFile filepath
        case C8.lines contents of
          (encSalt:_:encRest) ->
            case decodeBase64 encSalt of
              Left _ -> error "Decoding error"
              Right salt -> pure $ Vault salt $ decodeSecrets encRest
          _ -> error "Decoding error")
    (\(e :: IOException) -> do
       salt <- Hash.newSalt
       C8.writeFile filepath $
         encodeBase64' salt <> "\n----------------------------------\n"
       pure $ Vault salt [])

-- Inverts the list while decoding it
decodeSecrets :: [ByteString] -> [ByteString]
decodeSecrets input =
  case input of
    [] -> []
    (first:rest) ->
      case decodeBase64 first of
        Left _ -> do
          error "Decoding error"
        Right b -> (b : decodeSecrets rest)

getConfigFolder :: IO FilePath
getConfigFolder = getUserConfigDir "histcleaner"

getSecretsFilepath :: IO FilePath
getSecretsFilepath = getUserConfigFile "histcleaner" "secrets"
