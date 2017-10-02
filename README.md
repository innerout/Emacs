[![alt text](https://img.shields.io/badge/built%20with-Emacs-800080.svg "Emacs")](https://www.gnu.org/software/emacs/)

# Requirements 
* [Emacs](https://www.archlinux.org/packages/extra/i686/emacs/) based on the version of Arch linux.

### Arch

#### ```sudo pacman -S emacs```

### Ubuntu

#### ```sudo apt-get install emacs```
Probably it will work.
If after running install.sh emacs reports an error then uninstall emacs.Find a repository that has the latest emacs version import and download it after that you will have no problems probably.
If errors occur after that then contact me to help you.


### Fedora


#### ```sudo dnf install emacs```

## Installation

If you have reached here you got emacs installed by now.The only thing you have to do is to run these 3 commands and you will have the whole configuration running.

1. git clone https://github.com/innerout/Emacs.git
2. cd Emacs
3. ./install.sh

install.sh is running a python script that is setting up cedet in your home directory and then is backing up your .emacs and .emacs.d directory if any of them exists in a tar file in your home directory called **oldemacs.tar.gz** and lastly emacs.tar.gz is unpacked in your home directory.

After that you will have a fully working emacs IDE for C/C++ projects.
The plugins that i have already installed support more languages and with a few additions you could have great support for other languages too.

For example if you want python auto completion install **[JEDI](https://github.com/tkf/emacs-jedi)** and you got a python IDE.

## Installed plugins
1. [aggressive-indent](https://github.com/Malabarba/aggressive-indent-mode)
2. [cedet](http://cedet.sourceforge.net/)
3. [neotree](https://github.com/jaypei/emacs-neotree)
4. [helm](https://github.com/emacs-helm/helm)
5. [autopair](https://github.com/joaotavora/autopair)
6. [xcscope](https://github.com/dkogan/xcscope.el)
7. [indent-guide](https://github.com/zk-phi/indent-guide)
8. [yasnippet](https://github.com/joaotavora/yasnippet)
9. [autocomplete](https://github.com/auto-complete/auto-complete)
10. [autocomplete-c-headers](https://github.com/mooz/auto-complete-c-headers)
11. [The theme is spacegray](https://github.com/bruce/emacs-spacegray-theme)
12. [Color-identifiers-Mode](https://github.com/ankurdave/color-identifiers-mode)
13. [Origami](https://github.com/gregsexton/origami.el)
14. [Git-Gutter](https://github.com/syohex/emacs-git-gutter)
15. [Markdown-mode](https://jblevins.org/projects/markdown-mode/)(Requires pandoc to run)

## Notes
* Soon i will update with the macros and how to use them.
* Repo will be updated everytime i add a plugin to my configuration.
* I am searching for a good refactoring tool for the setup to be complete 
  after that i dont think any plugins will be needed (If i find anything usefull besides the refactoring tool i will add it).
