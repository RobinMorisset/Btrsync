#!/usr/bin/env python
import pstats
import argparse

def main():
  parser = argparse.ArgumentParser()
  parser.add_argument("--sort", help="column to sort", default="cum")
  parser.add_argument("stat_file", help="a stat file created by cprofile")
  args = parser.parse_args()

  stats = pstats.Stats(args.stat_file)
  stats.sort_stats(args.sort)
  stats.print_stats()

if __name__=="__main__":
  main()
