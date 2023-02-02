#!/usr/bin/env python3
import os
from argparse import ArgumentParser

"""
nice little script to point out areas in the
"""

# args
parser = ArgumentParser(
    prog = "80",
    description = "ensure code stays under 80 line length",
)
parser.add_argument('path')

args = parser.parse_args()

# collect sources
sources = []
for dirpath, dirnames, filenames in os.walk(args.path):
    for filename in filenames:
        sources.append(os.path.join(dirpath, filename))

# find lines that are too long
found = False
for source in sources:
    with open(source, 'r') as f:
        for i, line in enumerate(f.readlines()):
            if len(line) - 1 > 80:
                found = True
                print(f"{source}:{i + 1}")

exit(int(found))
