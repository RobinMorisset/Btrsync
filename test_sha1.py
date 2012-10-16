#!/usr/bin/env python

import btrsync
import cProfile
import pstats

#cProfile.run("btrsync.hash_dir()","fooprof")
#p = pstats.Stats('fooprof')
#p.sort_stats("cum")
#p.print_stats()

print btrsync.hash_dir()
