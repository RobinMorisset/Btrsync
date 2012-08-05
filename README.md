## Introduction

WARNING: This code is not stable yet. Do not use it unless you know what you are
doing.

btrsync is a program to synchronize files between two hosts, with detection of
move operations.

There are two independent implementations: one in Haskell and one in Python.
Besides, there is Python code for the move operations ("algo\_moves.py").

## Haskell implementation

To install for the current user only:
sudo apt-get install haskell-platform (can vary depending on your package
    manager. We actually only depend on GHC and Cabal)
cabal update
cabal install

To install for all users, replace "cabal install" by "sudo cabal install
--global")

To use, see btrsync --help.

## Python implementation

To use, install python-argparse and python-gmpy, install btrsync on both hosts,
and ensure that btrsync.py is in the PATH of both hosts.

See the relevant files for usage information.

