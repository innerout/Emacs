mv emacs.tar.gz ~/
python checkcedet.py
cd ~/
if ( [  -d ~/.emacs.d ] && [  -f ~/.emacs ] ) then 
   echo "Backing up your configuration in oldemacs.tar.gz"
   tar -cf oldemacs.tar.gz .emacs.d .emacs   
   elif([  -d ~/.emacs.d ]) then
       echo "Backing only your ~/.emacs.d folder in oldemacs.tar.gz  .emacs config does not exist"
       tar -cf oldemacs.tar.gz .emacs.d
   elif ([  -f ~/.emacs ]) then
	echo "Backing only your ~/.emacs config in oldemacs.tar.gz .emacs.d folder does not exist"
	tar -cf oldemacs.tar.gz .emacs
   fi
	tar -xf emacs.tar.gz
