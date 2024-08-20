module HistCleaner.Config where

import System.Environment.XDG.BaseDir
import System.FilePath

getConfigFolder :: IO FilePath
getConfigFolder = getUserConfigDir "histcleaner"

getEndInfosPath :: IO FilePath
getEndInfosPath = do
  configFolderPath <- getConfigFolder
  pure $ configFolderPath </> "endInfos"

getSecretsFilepath :: IO FilePath
getSecretsFilepath = getUserConfigFile "histcleaner" "secrets"
