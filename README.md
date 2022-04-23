# Requirements
My Emacs configuration is based on the [emacs-nativecomp](https://archlinux.org/packages/extra/x86_64/emacs-nativecomp/emacs-nativecomp) package although versions >= 27 should work for other distributions too.

### Dependencies

To install the dependencies for my configuration run:

	./install.sh

I have tested it in Arch as my daily driver and in WSL Ubuntu.

### Installation

``` bash
1. git clone https://github.com/innerout/Emacs.git
2. cp -r Emacs/.doom.d ~/
3. cd ~/
4. git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
5. ~/.emacs.d/bin/doom install
```
