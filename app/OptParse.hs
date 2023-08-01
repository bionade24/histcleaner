module OptParse
  ( Command(..)
  , SecretsCmd(..)
  , CleanOptions(..)
  , Files(..)
  , parse
  ) where

import Options.Applicative

-- | Model
data Command
  = ManageSecrets SecretsCmd
  | CleanFile CleanOptions Files
  deriving (Show)

data SecretsCmd
  = AddSecret
  | RemoveSecret
  deriving (Show)

data CleanOptions =
  CleanOptions
    { force :: Bool
    }
  deriving (Show)

newtype Files =
  Files [FilePath]
  deriving (Show)

parse :: IO Command
parse = execParser cmds

cmds :: ParserInfo Command
cmds =
  info
    (pCommand <**> helper)
    (fullDesc <> header "histcleaner" <> progDesc "sanitize a file from secrets")

pCommand :: Parser Command
pCommand =
  subparser
    (command
       "secret"
       (info
          (helper <*> pManageSecrets)
          (progDesc "Manage the secrets database")) <>
     command
       "clean"
       (info (helper <*> pCleanFile) (progDesc "Clean history file of secrets")))

pManageSecrets :: Parser Command
pManageSecrets =
  ManageSecrets <$>
  subparser
    (command
       "add"
       (info (helper <*> pure AddSecret) (progDesc "AddSecret a secret")) <>
     command
       "remove"
       (info (helper <*> pure RemoveSecret) (progDesc "RemoveSecret a secret")))

pCleanFile :: Parser Command
pCleanFile = CleanFile <$> pCleanOptions <*> pHistFile

pCleanOptions :: Parser CleanOptions
pCleanOptions =
  CleanOptions <$>
  switch
    (long "force" <>
     short 'f' <> help "Ignore known end position and check the whole file.")

pHistFile :: Parser Files
pHistFile = Files <$> parser
  where
    parser = some (argument str (metavar "FILES..."))
