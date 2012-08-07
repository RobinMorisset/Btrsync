#!/usr/bin/env python2
from __future__ import print_function

"""Perform a series of moves in-place on a filesystem hierarchy"""
__author__ = "Antoine Amarilli"

import os
import os.path
import shutil
import sys
import tempfile

from sha1 import sha1

def do_moves(base, moves):
  """Perform moves in base
 
  - base is the folder on which the moves should be performed
  - moves[f] should be the list of locations where f has to be moved

  It is asserted that only files are being moved, and that if two files are
  moved to the same location then they must have the same content. There is no
  support for symlinks. There is no support for file metadata (dates,
  permissions, ownership, etc.)"""

  def rebase(f):
    if f.startswith('/'):
      # absolute path, for a temporary file
      return f
    else:
      # relative path
      return base + f

  def mkdir(d):
    os.mkdir(rebase(d))
  def mkdirs(f):
    """make all required directories (like mkdir -p) to put a file in f"""
    d = f.split('/')
    for i in range(len(d)):
      target = '/'.join(d[:i])
      if not isdir(target):
        mkdir(target)
  def copy(f, t):
    print("copy %s %s" % (f, t), file=sys.stderr)
    mkdirs(t)
    shutil.copy(rebase(f), rebase(t))
  def move(f, t):
    print("move %s %s" % (f, t), file=sys.stderr)
    mkdirs(t)
    shutil.move(rebase(f), rebase(t))
  def unlink(f):
    print("unlink %s" % f, file=sys.stderr)
    os.unlink(rebase(f))
  def exists(f):
    print("exists %s" % (f), file=sys.stderr)
    return os.path.exists(rebase(f))
  def isdir(f):
    return os.path.isdir(rebase(f))
  def isfile(f):
    return os.path.isfile(rebase(f))
  def checksum(f):
    return sha1(rebase(f))

  def blocker(destination):
    """is there a file preventing something to go to destination?"""
    d = destination.split('/')
    for i in range(len(d)):
      candidate = '/'.join(d[:i+1])
      if exists(candidate) and not isdir(candidate):
        return candidate
    return None

  def manage_move(f, destination, last_one=False):
    """move file to destination, managing possible blocker

    last_one indicates if this is the last operation on file (ie. we should move
    it instead of copying it)"""

    b = blocker(destination)
    print ("blocker %s %s" % (destination, b), file=sys.stderr)
    dummy = False
    if b:
      if b not in moves.keys():
        # b is a blocker but does not have to be moved anywhere
        # we can just delete it
        unlink(b)
      else:
        if color[b] in [DONE, MOVED]:
          # theoretically, there is no need to perform anything
          dummy = True
        else:
          manage(b)
    if color[f] == MOVED:
      # someone has moved f, we must work with the temporary copy
      f2 = temps[f]
    else:
      f2 = f
    if dummy:
      assert(isfile(f2) and isfile(destination)
          and checksum(f2) == checksum(destination))
    if last_one:
      if not dummy:
        move(f2, destination)
      else:
        # we still have to delete
        unlink(f2)
    else:
      if not dummy:
        copy(f2, destination)

  def manage(f):
    """manage moves for a file"""
    if color[f] in [DONE, MOVED]:
      # f has already been dealt with
      return
    if color[f] == DOING:
      # f is being dealt with, we have found a cycle
      # we move it out of the way to a temporary location
      temp = tempfile.NamedTemporaryFile(delete=False)
      temp.close()
      move(f, temp.name)
      # we mark it as done, this ensures two things:
      # 1. other moves to the same location will not be performed, which is OK
      # because they should come from identical files)
      # 2. the parent call of manage on f will notice the move
      color[f] = MOVED
      # indicate the temp file for f to the parent call
      temps[f] = temp.name
      return
    color[f] = DOING
    destinations = moves[f]
    seen_self = False
    for i in range(len(destinations)):
      if destinations[i] == f:
        seen_self = True
        continue
      manage_move(f, destinations[i],
        last_one=(i == len(destinations) - 1))
    if color[f] == MOVED and seen_self:
      # we must put back f in its original place
      move(temps[f], f)
    color[f] = DONE

  # color contains the possible colors for a file
  color = {}
  # the colors are as follows:
  # - UNTOUCHED: no operation has taken place
  # - DOING: the file is being dealt with
  # - MOVED: the file has been dealt with and moved out of the way temporarily
  # because of a cycle
  # - DONE: the file has been dealt with
  UNTOUCHED, DOING, MOVED, DONE = range(4)
  for k in moves.keys():
    color[k] = UNTOUCHED
  # temps contain the temporary name for files moved out of the way
  temps = {}

  # trailing '/' for base
  if not base.endswith('/'):
    base += '/'

  # perform the moves
  for file in moves.keys():
    manage(file)

if __name__ == '__main__':
  if len(sys.argv) != 2:
    print ("Usage: %s DIRECTORY" % sys.argv[0], file=sys.stderr)
    print ("Read 'FROM\\tTO' lines on stdin, perform them on DIRECTORY"
        % sys.argv[0], file=sys.stderr)
    sys.exit(1)
  base = sys.argv[1]
  moves = {}
  while True:
    l = sys.stdin.readline()
    if not l:
      break
    l = l.strip().split('\t')
    if l[0] not in moves.keys():
      moves[l[0]] = []
    moves[l[0]].append(l[1])
  do_moves(base, moves)

