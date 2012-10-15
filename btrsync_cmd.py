#!/usr/bin/env python2
from __future__ import print_function

__author__ = "Antoine Amarilli and Fabrice Ben Hamouda"

import os, re
import argparse

def shellquote(s):
  return "'" + s.replace("'", "'\\''") + "'"

def main():
  parser = argparse.ArgumentParser(description="WARNING: Internal use. Please use btrsync.sh")
  parser.add_argument("--status", help="Status file to write (internal use)")
  parser.add_argument("root_neil", help="[[user@]host:]path/to/neil")
  parser.add_argument("root_oscar", help="[[user@]host:]path/to/oscar")
  args = parser.parse_args()

  # Print command to be executed for Neil and Oscar
  # Used by btrsync.sh
  print ("ok")

  regex = re.compile("^((?P<server>[^:]+):)?(?P<path>.*)$")
  r_oscar = regex.search(args.root_oscar).groupdict()
  r_neil = regex.search(args.root_neil).groupdict()

  if r_neil["server"] == None:
    root_neil = os.path.abspath(args.root_neil)
    root_neil_local = root_neil
  else:
    root_neil = args.root_neil
    root_neil_local = r_neil["path"]

  if r_oscar["server"] == None:
    root_oscar = os.path.abspath(args.root_oscar)
    root_oscar_local = root_oscar
  else:
    root_oscar = args.root_oscar
    root_oscar_local = r_oscar["path"]

  if r_neil["server"]==None:
    print ("btrsync.py --origin %s %s" % (shellquote(root_neil_local), shellquote(root_oscar)))
  else:
    print ("ssh %s btrsync.py --origin %s %s" % (r_neil["server"], shellquote(root_neil_local), shellquote(root_oscar)))

  # if a status file is provided, pass it to the destination:
  invocation = "btrsync.py %s --destination" % ("--status=" + shellquote(args.status) if args.status else "")
  if r_oscar["server"]==None:
    print ("%s %s %s" % (invocation, shellquote(root_neil), shellquote(root_oscar_local)))
  else:
    print ("ssh %s %s %s %s" % (r_oscar["server"], invocation, shellquote(root_neil), shellquote(root_oscar_local)))

if __name__ == "__main__":
  main()
