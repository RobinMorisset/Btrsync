import hashlib

def sha1(f):
  """return the sha1 hash of file of path f"""
  sha1 = hashlib.sha1()
  fp = open(f, 'rb')
  try:
    while True:
      buf = fp.read(16*1024*1024)
      if not buf:
        break
      sha1.update(buf)
  finally:
    fp.close()
  return sha1.hexdigest()
