#!/usr/bin/env python2
from __future__ import print_function

"""Btrsync (python version)"""
__author__ = "Antoine Amarilli and Fabrice Ben Hamouda"

import os, sys, inspect
import shutil
from subprocess import Popen, PIPE, STDOUT
import argparse
import hashlib
import gmpy
from gmpy import mpz
import json
import itertools
import multiprocessing as mp

# Enable us to use the modules in the same folder than the script
cmd_folder = os.path.realpath(os.path.abspath(os.path.split(inspect.getfile( inspect.currentframe() ))[0]))
if cmd_folder not in sys.path:
  sys.path.insert(0, cmd_folder)

from sha1 import sha1
from algo_moves import do_moves

P_SIZE = 1024
HASH_SIZE = 160
SEED = 10
PROFILE = True

if PROFILE:
  import cProfile
  import pstats

# JSON encoder for mpz (gmpy big integer)
class MpzJSONEncoder(json.JSONEncoder):
  def default(self, obj):
    if obj.__class__.__name__=="mpz":
      return {'_mpz': int(obj)}
    else:
      return JSONEncoder.default(self, obj)

def as_python_object(dct):
  if '_mpz' in dct:
    return mpz(dct["_mpz"])
  return dct

def eprint(a):
  print(a, file=sys.stderr)

def send(a):
  j = json.dumps(a, cls=MpzJSONEncoder)
  print(j)
  sys.stdout.flush()
  eprint("Send " + j)
  return len(j)

def receive():
  j = sys.stdin.readline()
  return json.loads(j, object_hook=as_python_object), len(j)

def hash_to_prime(h):
  # TODO: improve this !
  return gmpy.next_prime((h & ((1<<(HASH_SIZE-16+1)) - 1)) << 16)


def hash_file(path, other=""):
  """ Return (h,hcontents) where:
          h is a hash of the path, the string other and the content to a prime
          hcontent is a classical hash of the content
      TODO: add metadata """
  hcontent = sha1(path)
  h = int(hashlib.sha1('f\0'+path+'\0'+other+'\0'+hcontent).hexdigest(), 16)
  hcontent = int(hcontent, 16)
  return (hash_to_prime(h), hcontent)

def hash_dirname(path, other=""):
  h = int(hashlib.sha1('d\0'+path+'\0'+other).hexdigest(), 16)
  return hash_to_prime(h)

def hash_dir_file((path,isdir)):
  if isdir:
    h = hash_dirname(path)
    return (h,(path,True,None))
  else:
    (h,hcontent) = hash_file(path)
    return (h,(path,False,hcontent))

def hash_dir():
  def list_dirs_files((root,dirs,files)):
    def f_dir(dirname):
      path = root+'/'+dirname
      #h = hash_dirname(path)
      #hashes[h] = (path, True, None)
      return (path,True)
    def f_file(filename):
      path = root+'/'+filename
      #(h, hcontent) = hash_file(path)
      #hashes[h] = (path, False, hcontent)
      return (path,False)
    return itertools.chain(itertools.imap(f_dir,dirs),
      itertools.imap(f_file,files))

  dirs_files = itertools.chain.from_iterable(itertools.imap(list_dirs_files,os.walk('.'))) # flatten !

  # TODO: mp.Pool().imap should be better, but I do not nknow why it does not work...
  #hashes = list(dirs_files)
  hashes = mp.Pool().map(hash_dir_file,dirs_files)
  #hashes = map(hash_dir_file,dirs_files)

  #return hashes
  return dict(hashes)

def p_gen():
  small_p = mpz(1)
  k = P_SIZE
  while True:
    small_p = gmpy.next_prime(small_p)
    p = small_p ** int(k / (small_p.bit_length()-1))
    yield p
    k *= 2
p_gen_inst = p_gen()

#import time
def generate_next_p():
  """ Generate the i^th number modulo p """
#  a = time.time()
  p = p_gen_inst.next()
#  b = time.time()
#  eprint("%d, time: %f" % (int(p), a-b))
  return p

def product_mod(n, l):
  """ Compute the product of the elements of l modulo n """
  return reduce(lambda x, y: ((x * y) % n), l, 1) % n

def make_frac(n, d):
  a = n
  b = d % n
  x = 0
  y = 1
  lastx = 1
  lasty = 0
  nn = n.numdigits(2)
  stop = gmpy.mpz(1) << (nn/2)
  while b > stop:
    q = a / b
    r = a % b
    a = b
    b = r
    (x, lastx) = (lastx - q * x, x)
    (y, lasty) = (lasty - q * y, y)
  return (b, y%n)

def factor(a, l):
  """ Factor a on the basis l (list of primes), return the list of factors or None, if not completely factorized """
  factors = []
  for p in l:
    if a % p == 0:
      a = a / p
      factors.append(p)
  if a == 1:
    return factors
  else:
    return None


def round_neil(hashes, pi_oscar, prev_pp=1, prev_d=1):
  p = generate_next_p()
  pi_neil = product_mod(p, hashes)
  d = pi_oscar * gmpy.invert(pi_neil, p)

  # TODO CRT d/prev_d
  d = d * prev_pp * gmpy.invert(prev_pp, p) + prev_d * p * gmpy.invert(p, prev_pp)
  pp = p * prev_pp

  (oscar, neil) = make_frac(pp, d)
  factors_neil = factor(neil, hashes)

  if factors_neil != None:
    files_neil = [hashes[h] for h in factors_neil]
    return (pp, d, (oscar, files_neil))
  else:
    return (pp, d, None)

def sync_oscar(root_neil, root_oscar, files_neil, files_oscar):
  """ Actually perform the synchronisation, given a list of files in Oscar and not in Neil, and a list of files in Neil and not in Oscar """
  # TODO: manage metadata
  files_by_path_oscar = {}
  files_by_content_oscar = {} # future -> for moves
  files_by_path_neil = {}

  # Do moves !
  for (path, isdir, hcontent) in files_oscar:
    if not isdir:
      files_by_path_oscar[path] = hcontent
      files_by_content_oscar[hcontent] = path

  moves = {}
  files_treated_neil = set()
  for (path, isdir, hcontent) in files_neil:
    if not isdir:
      if files_by_path_oscar.get(path) != hcontent:
        # we need to do a move if possible
        if files_by_content_oscar.has_key(hcontent):
          files_treated_neil.add(path)
          orig = files_by_content_oscar[hcontent]
          if moves.has_key(orig):
            moves[orig].append(path)
          else:
            moves[orig] = [path]
  # TODO: not optimal because do_moves recomputes hashes, but...
  #eprint("moves")
  #eprint(moves)
  do_moves(root_oscar, moves)

  # Remove folders and create indexes...
  for (path, isdir, hcontent) in files_oscar:
    if isdir:
      if os.path.isdir(path):
        # We may have already removed the path or the path may have become a file, because of previous moves
        # TODO: Metadata...
        shutil.rmtree(path)

  # Create folders and create indexes
  for (path, isdir, hcontent) in files_neil:
    if isdir:
      if os.path.isdir(path):
        pass
      elif os.path.isfile(path):
        os.unlink(path)
        os.makedirs(path)
      else:
        os.makedirs(path)
    else:
      files_by_path_neil[path] = True

  # Remove files not in common
  for (path, isdir, hcontent) in files_oscar:
    if not isdir:
      if os.path.isfile(path):
        if not files_by_path_neil.has_key(path):
          os.unlink(path)

  # Synchronize the other files
  rsync = Popen(['rsync', '-Iv', '--files-from=-', root_neil, root_oscar],
      stdin=PIPE, stdout=PIPE)
  for (path, isdir, hcontent) in files_neil:
    if not isdir:
      if path not in files_treated_neil:
        #eprint(path)
        rsync.stdin.write(path+"\n")
  rsync.stdin.close()
  # Parse rsync statistics !
  last1 = None
  last2 = None
  line = None
  while True:
    last1, last2 = line, last1
    line = rsync.stdout.readline()
    if not line:
      break
  fields = last2.split(' ')
  assert(fields[0] == 'sent' and fields[2] == 'bytes'
      and fields[4] == 'received' and fields[6] == 'bytes')
  sent = int(fields[1])
  received = int(fields[5])
  return sent, received

def main_neil(args):
  # Neil
  eprint("Neil: start")
  os.chdir(args.root_neil)
  hashes = hash_dir()
  eprint("Neil: hash_dir finished")

  pp = 1
  d = 1
  res_neil = None
  while res_neil == None:
    pi_oscar, size = receive()
    (pp, d, res_neil) = round_neil(hashes, pi_oscar, pp, d)
    send(res_neil)

def main_oscar(args):
  # Oscar
  eprint("Oscar: start")
  os.chdir(args.root_oscar)
  hashes = hash_dir()
  eprint("Oscar: hash_dir finished")

  res_neil = None
  my_sent = 0
  my_received = 0

  while res_neil == None:
    p = generate_next_p()
    pi_oscar = product_mod(p, hashes)
    my_sent += send(pi_oscar)
    res_neil, my_nreceived = receive()
    my_received += my_nreceived

  (oscar, files_neil) = res_neil
  factors_oscar = factor(oscar, hashes)
  files_oscar = [hashes[h] for h in factors_oscar]
  #eprint("Neil files:")
  #eprint(files_neil)
  #eprint("Oscar files:")
  #eprint(files_oscar)
  sent, received = sync_oscar(args.root_neil, args.root_oscar, files_neil, files_oscar)
  if args.status:
    status = open(args.status, 'w')
    print ("accounting_btrsync: sent %d bytes  received %d bytes" % (my_sent, my_received), file=status)
    print ("accounting_rsync: sent %d bytes  received %d bytes" % (sent, received), file=status)
    print ("accounting_total: sent %d bytes  received %d bytes" % (
      sent + my_sent, received + my_received), file=status)
    status.close()

def main():
  parser = argparse.ArgumentParser(description="WARNING: Internal use. Please use btrsync.sh")
  parser.add_argument("--origin", help="Play Neil role", action="store_true")
  parser.add_argument("--destination", help="Play Oscar role", action="store_true")
  parser.add_argument("--status", help="Status file to write (internal use)")
  parser.add_argument("root_neil", help="[[user@]host:]path/to/neil")
  parser.add_argument("root_oscar", help="[[user@]host:]path/to/oscar")
  args = parser.parse_args()

  if args.origin:
    if PROFILE:
      cProfile.runctx("main_neil(args)",globals(),locals(),"/tmp/btrsync_profile_neil")
    else:
      main_neil(args)
  elif args.destination:
    if PROFILE:
      cProfile.runctx("main_oscar(args)",globals(),locals(),"/tmp/btrsync_profile_oscar")
    else:
      main_oscar(args)
  else:
    args.error("No role specified, add --origin or --destination.")

  # Fix: http://stackoverflow.com/questions/7955138/addressing-sys-excepthook-error-in-bash-script
  try:
    sys.stdout.close()
  except:
    pass
  try:
    sys.stderr.close()
  except:
    pass




if __name__ == "__main__":
  #Just a Unit test
  assert make_frac(gmpy.mpz(102577), gmpy.mpz(101)*gmpy.invert(gmpy.mpz(125),
    102577)) == (gmpy.mpz(101), gmpy.mpz(125))
  main()
