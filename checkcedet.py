#!/bin/python
import os
import time 
from pathlib import Path

print("Cedet check is starting\n")

if(not os.path.isdir(str(Path.home())+"/cedet")):
    home=str(Path.home())
    os.chdir(home)
    os.system('git clone http://git.code.sf.net/p/cedet/git cedet')
    home=str(Path.home())+"/cedet"
    os.chdir(home)
    os.system('make')
    home=str(Path.home())+"/cedet/contrib"
    os.chdir(home)
    os.system('make')

print("Cedet is configured proceeding to uncompressing emacs tar\n")
time.sleep(5)
