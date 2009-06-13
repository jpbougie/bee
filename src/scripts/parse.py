#!/usr/bin/env python
# encoding: utf-8
"""
parse.py

Created by Jean-Philippe Bougie on 2009-05-21.
Copyright (c) 2009 __MyCompanyName__. All rights reserved.
"""

import sys
import os
import subprocess
import tempfile

PATH_SPLITTER = "/Users/jp/Projects/bee/dist/bee-0.1/word-splitter.pl"
PATH_POS = "/Users/jp/Projects/Thesis/tools/illinois/newPos"
PATH_SP = "/Users/jp/Projects/Thesis/tools/illinois/shallow-parser.RO/ShallowParser"


def main():
  """ Reads a sentence from stdin, parses it and outputs it back tagged to stdout"""
  for line in sys.stdin:
    temp1 = tempfile.NamedTemporaryFile()
    temp1.write(line)
    temp1.flush()
    
    # sanitizer
    p = subprocess.Popen((PATH_SPLITTER, temp1.name), stdout=subprocess.PIPE)
    out, error = p.communicate()
    temp2 = open("input.txt", "w")
    temp2.write(out)
    temp2.close()    
    
    # tagger
    subprocess.call(PATH_POS + "/tagger -i " + os.path.abspath(temp2.name) + " -o output.txt", shell=True, env={"PATH_POS": PATH_POS})
    
    # parser
    p = subprocess.Popen((PATH_SP + "/bin/callChunkerServer.pl", "output.txt"), stdout=subprocess.PIPE, env={"PATH_SP": PATH_SP})
    out, error = p.communicate()
    
    print out

if __name__ == '__main__':
  main()

