module OptParse
  ( Command(..)
  , SecretsCmd(..)
  , SecretsOptions(..)
  , CleanOptions(..)
  , Files(..)
  , parse
  ) where

import Options.Applicative

-- | Model
data Command
  = ManageSecrets SecretsOptions SecretsCmd
  | CleanFile CleanOptions Files
  deriving (Show)

data SecretsCmd
  = AddSecret
  | RemoveSecret
  deriving (Show)

data SecretsOptions =
  SecretsOptions
    { keep :: Bool
    }
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
  ManageSecrets <$> pSecretsOptions <*>
  subparser
    (command "add" (info (helper <*> pure AddSecret) (progDesc "Add a secret")) <>
     command
       "remove"
       (info (helper <*> pure RemoveSecret) (progDesc "Remove a secret")))

pSecretsOptions :: Parser SecretsOptions
pSecretsOptions =
  SecretsOptions <$>
  switch (long "keep" <> short 'k' <> help "Always keep remembered endLines.")

pCleanFile :: Parser Command
pCleanFile = CleanFile <$> pCleanOptions <*> pHistFile

pCleanOptions :: Parser CleanOptions
pCleanOptions =
  CleanOptions <$>
  switch
    (long "force" <>
     short 'f' <>
     help "Ignore known previous end position and check the whole file.")

pHistFile :: Parser Files
pHistFile = Files <$> parser
  where
    parser = some (argument str (metavar "FILES..."))
