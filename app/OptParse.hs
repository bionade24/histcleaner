module OptParse
  ( Command(..)
  , SecretsCmd(..)
  , Files(..)
  , parse
  )
  where

import Options.Applicative


-- | Model
data Command
  = ManageSecrets SecretsCmd
  | CleanFile Files
  deriving Show

data SecretsCmd
  = AddSecret
  | RemoveSecret
  deriving Show

newtype Files
  = Files [FilePath]
  deriving Show

parse :: IO Command
parse =
  execParser opts

opts :: ParserInfo Command
opts =
  info (pCommand <**> helper)
    ( fullDesc
      <> header "histcleaner"
      <> progDesc "remove your secrets from a file"
    )

pCommand :: Parser Command
pCommand =
  subparser
    ( command
      "secret"
      ( info
        (helper <*> pManageSecrets)
        (progDesc "Manage the secrets database")
      )
      <> command
      "clean"
      ( info
        (helper <*> pCleanFile)
        (progDesc "Clean history file of secrets")
      )
    )

pManageSecrets :: Parser Command
pManageSecrets =
  ManageSecrets <$> subparser
    ( command
      "add"
      ( info
        (helper <*> pure AddSecret)
        (progDesc "AddSecret a secret")
      )
      <> command
      "remove"
      ( info
        (helper <*> pure RemoveSecret)
        (progDesc "RemoveSecret a secret")
      )
    )

pCleanFile :: Parser Command
pCleanFile =
  CleanFile <$> pHistFile

pHistFile :: Parser Files
pHistFile = Files <$> parser
  where
    parser =
      some
        ( argument
          str
          ( metavar "FILES...")
        )
