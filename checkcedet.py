#!/bin/python
from tkinter import *
import os
from pathlib import Path

root=Tk()
w=Label(root,text="Emacs will test if cedet exists!\n \
 if its not it will download and compile it\n \
")
w.pack()
root.after(1000,lambda:root.destroy())
root.mainloop()
if(not os.path.isdir("~/cedet")):
    home=str(Path.home())
    os.chdir(home)
    os.system('git clone http://git.code.sf.net/p/cedet/git cedet')
    home=str(Path.home())+"/cedet"
    os.chdir(home)
    os.system('make')
    home2=str(Path.home())+"/cedet/contrib"
    os.chdir(home2)
    os.system('make')
