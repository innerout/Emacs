mv emacs ~/.emacs
dir="$HOME/cedet"
echo "Cedet check is starting\n"
if [ ! -d "$dir" ]; then
	cd ~/
	git clone http://git.code.sf.net/p/cedet/git cedet
	cd cedet
	make
	cd contrib
	make
fi
echo "Cedet is configured proceeding to setup .emacs\n"
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
