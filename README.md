Histcleaner - Sanitize secrets from your files
==

Command line tool that sanitizes your (history) files of your secrets like passwords or tokens, so you can rest in calm without looking through those files yourself :)
Secrets to be sanitized have to be added to the database of the tool, which stores an Argon2id hash of them,
so that the tool can't leak your secrets itself.
Those hashes are then compared to words in the file you clean and the secrets get redacted.
The initial run may take a while, but the tool will resume at the last known location on the next run.

Currently supports plain text files like your shell's histfile and `.lesshst`.

```
$ histcleaner -h
histcleaner

Usage: histcleaner COMMAND

  remove your secrets from a file

Available options:
  -h,--help                Show this help text

Available commands:
  secret                   Manage the secrets database
  clean                    Clean history file of secrets
```

Source installation instructions
=
1. Install ghc & cabal
2. `git clone --single-branch https://github.com/bionade24/histcleaner.git`
3. `cd histcleaner && cabal update && cabal build`
4. `install -m755 $(find -type f -executable -name histcleaner) /usr/local/bin/histcleaner`
