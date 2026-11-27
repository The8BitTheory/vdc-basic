#!/usr/bin/env python3
"""Copy the listings into the chapters.

A chapter marks a listing with a line  <!-- listing: NAME.bas -->  followed by a
fenced code block. This script replaces the content of that block with the file
docs/listings/NAME.bas, so the text always shows exactly what was tested.

usage: docs/listings/sync.py        (rewrites docs/*.md in place)
"""
import glob, os, re

HERE = os.path.dirname(os.path.abspath(__file__))
PAT = re.compile(r"(<!-- listing: ([\w.-]+) -->\n```\n).*?(```)", re.S)

for md in glob.glob(os.path.join(HERE, "..", "*.md")):
    text = open(md, encoding="utf-8").read()
    new = PAT.sub(lambda m: m.group(1) + open(os.path.join(HERE, m.group(2)), encoding="utf-8").read() + m.group(3), text)
    if new != text:
        open(md, "w", encoding="utf-8").write(new)
        print("updated", os.path.basename(md))
