## Introduction

WARNING: This code is not stable yet. Do not use it unless you know what you are
doing.

btrsync is a program to synchronize files between two hosts, with detection of
move operations.

There are two independent implementations: one in Haskell and one in Python. The
Haskell implementation is not stable and is documented in its own section. The
rest of this document describes the Python implementation.

## Credits and license

The Python implementation of btrsync is (C) 2012 Fabrice Ben Hamouda and
Antoine Amarilli.

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, version 3.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program (see file "COPYING").  If not, see <http://www.gnu.org/licenses/>.

## Installation

Install python-argparse and python-gmpy, install btrsync on both hosts,
and ensure that:
- either all \*.py files are in the PATH of both hosts.
- or btrsync.py is in the PATH of both hosts and the other \*.py files are in
  the PYTHONPATH of both hosts

## Usage

You should not run btrsync.py directly. Please always run btrsync.sh.

For help, run:

    btrsync.sh -h

## Testing

Unit tests are provided. To run them:

    cd tests
    ./tests.sh

## Haskell implementation

Remember that this implementation is not stable and should not be used directly.

To install for the current user only:
sudo apt-get install haskell-platform (can vary depending on your package
    manager. We actually only depend on GHC and Cabal)
cabal update
cabal install

To install for all users, replace "cabal install" by "sudo cabal install
--global")

To use, see btrsync --help.

