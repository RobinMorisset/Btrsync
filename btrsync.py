#!/usr/bin/python

import hashlib
import os
import gmpy
from gmpy import mpz
import operator
import argparse
import sys
import time
import shutil
from subprocess import Popen, PIPE, STDOUT

P_SIZE    = 1024
HASH_SIZE = 160
SEED      = 10 

# WARNING: use the random number generator only for generate_next_p !

gmpy.rand('init')
gmpy.rand('seed', SEED)

def eprint(a):
  if isinstance(a, basestring) == False:
    a = repr(a)
  sys.stderr.write(a+'\n')

def send(a):
  print repr(a)
  sys.stdout.flush()
  eprint("Send " + repr(a))

def hash_to_prime(h):
  # TODO: improve this !
  return gmpy.next_prime((h & ((1<<(HASH_SIZE-16+1)) - 1)) << 16)


def hash_file(path,other=""):
  """ Return (h,hcontents) where:
          h is a hash of the path, the string other and the content to a prime
          hcontent is a classical hash of the content
      TODO: add permissions """
  f = file(path)
  content = f.read()
  h = int(hashlib.sha1('f\0'+path+'\0'+other+'\0'+content).hexdigest(), 16)
  hcontent = int(hashlib.sha1(content).hexdigest(), 16)
  f.close()
  return (hash_to_prime(h), hcontent)

def hash_dirname(path,other=""):
  h = int(hashlib.sha1('d\0'+path+'\0'+other).hexdigest(), 16)
  return hash_to_prime(h)

def hash_dir():
  hashes = {}
  for root, dirs, files in os.walk('.'):
    for dirname in dirs:
      path = root+'/'+dirname
      h = hash_dirname(path)
      hashes[h] = (path,True,None)
    for filename in files:
      path = root+'/'+filename
      (h,hcontent) = hash_file(path)
      hashes[h] = (path,False,hcontent)
  return hashes

def generate_next_p():
  """ Generate the i^th number p """
  def rand_bits(bits):
    """ Pick a random number of n bits """
    n = gmpy.rand('next',(1<<(bits+1))-1)
    if n < (1<<bits):
      return rand_bits(bits)
    else:
      return n

  return gmpy.next_prime(rand_bits(P_SIZE))

def product_mod(n,l):
  """ Compute the product of the elements of l modulo n """
  return reduce(lambda x,y: ((x * y) % n), l) % n

def make_frac(n,d):
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
  return (b,y%n)

def factor(a,l):
  """ Factor a on the basis l (list of primes), return the list of factors or None, if not completely factorized """
  factors = []
  for p in l:
    if a % p == 0:
      a = a / p
      factors.append(p)
  if a==1:
    return factors
  else:
    return None


def round_neil(hashes,pi_oscar,prev_pp=1,prev_d=1):
  p = generate_next_p()
  pi_neil = product_mod(p, hashes)
  d = pi_oscar * gmpy.invert(pi_neil, p)

  # TODO CRT d/prev_d
  d = d * prev_pp * gmpy.invert(prev_pp,p) + prev_d * p * gmpy.invert(p,prev_pp)
  pp = p * prev_pp

  (oscar,neil) = make_frac(pp,d)
  factors_neil = factor(neil, hashes)

  if factors_neil != None:
    files_neil = [hashes[h] for h in factors_neil]
    return (pp,d,(oscar, files_neil))
  else:
    return (pp,d,None)

def sync_oscar(root_neil, root_oscar, files_neil, files_oscar):
  """ Actually perform the synchronisation, given a list of files in Oscar and not in Neil, and a list of files in Neil and not in Oscar """
  # TODO: can still be optimize, we should take advantage of moves
  # TODO: remote...
  files_by_path_oscar = {}
  files_by_content_oscar = {} # future -> for moves
  files_by_path_neil = {}

  # Remove folders and create indexes...
  for (path,isdir,hcontent) in files_oscar:
    if isdir==True:
      if os.path.isdir(path):
        # We may have already removed the path -> the test (can be optimize by sorting the paths maybe)
        shutil.rmtree(path)
    else:
      files_by_path_oscar[path] = True
      files_by_content_oscar[hcontent] = True

  # Create folders and create indexes
  for (path,isdir,hcontent) in files_neil:
    if isdir==True:
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
  for (path,isdir,hcontent) in files_oscar:
    if isdir==False:
      if os.path.isfile(path):
        if files_by_path_neil.has_key(path) == False:
          os.unlink(path)

  # Synchronize the other files
  rsync = Popen(['rsync', '--files-from=-',root_neil,root_oscar], stdout=sys.stderr, stdin=PIPE, stderr=STDOUT)
  for (path,isdir,hcontent) in files_neil:
    if isdir==False:
      rsync.stdin.write(path+"\n")
  rsync.communicate()[0]
  rsync.stdin.close()

def main():
  parser = argparse.ArgumentParser()
  parser.add_argument("--origin", help="", action="store_true")
  parser.add_argument("--destination", help="", action="store_true")
  parser.add_argument("root_neil", help="")
  parser.add_argument("root_oscar", help="")
  args = parser.parse_args()

  root_neil = os.path.abspath(args.root_neil)
  root_oscar = os.path.abspath(args.root_oscar)

  if args.origin:
    # Neil
    eprint("start Neil")
    os.chdir(root_neil)
    hashes = hash_dir()

    pp = 1
    d = 1
    res_neil = None
    while res_neil==None:
      pi_oscar = eval(sys.stdin.readline())
      (pp,d,res_neil) = round_neil(hashes,pi_oscar,pp,d)
      send(res_neil)

  elif args.destination:
    # Oscar
    eprint("start Oscar")
    os.chdir(root_oscar)
    hashes = hash_dir()

    res_neil = None
    while res_neil==None:
      p = generate_next_p()
      pi_oscar = product_mod(p,hashes)
      send(pi_oscar)
      res_neil = eval(sys.stdin.readline())

    (oscar, files_neil) = res_neil
    factors_oscar = factor(oscar, hashes)
    files_oscar = [hashes[h] for h in factors_oscar]
    eprint("Neil files:")
    eprint(files_neil)
    eprint("Oscar files:")
    eprint(files_oscar)
    sync_oscar(root_neil,root_oscar,files_neil,files_oscar)


  else:
    print "btrsync.py --origin %s %s" % (root_neil,root_oscar)
    print "btrsync.py --destination %s %s" % (root_neil,root_oscar)


hashes = hash_dir()
#print repr(product_mod(gmpy.mpz(1000),hashes))
#print generate_next_p()
assert make_frac(gmpy.mpz(102577),gmpy.mpz(101)*gmpy.invert(gmpy.mpz(125),102577)) == (gmpy.mpz(101),gmpy.mpz(125))

if __name__=="__main__":
  main()
