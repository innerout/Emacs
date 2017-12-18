[![GNU Emacs](https://img.shields.io/badge/built%20with-Emacs-800080.svg "Emacs")](https://www.gnu.org/software/emacs/)
[![AUR](https://img.shields.io/aur/license/yaourt.svg)](https://opensource.org/licenses/GPL-2.0/)

# Requirements
* [Emacs](https://www.archlinux.org/packages/extra/i686/emacs/) based on the version of Arch linux.

### Arch

#### ```sudo pacman -S emacs```

### Ubuntu/Debian

#### ```sudo add-apt-repository ppa:kelleyk/emacs```
#### ```sudo apt-get update```
#### ```sudo apt-get install emacs25```

### Fedora

#### ```sudo dnf install emacs```

## Installation

If you have reached here you got emacs installed by now.The only thing you have to do is to run these 3 commands and you will have the whole configuration running.

1. git clone https://github.com/innerout/Emacs.git
2. cd Emacs
3. ./install.sh

install.sh is backing up your .emacs and .emacs.d directory if any of them exists in a tar file in your home directory called **oldemacs.tar.gz** and is copying emacs to ~/.emacs.

After that you will have a fully working emacs IDE for C/C++/Python projects.

The plugins i have already installed support more languages and with a few additions you could have great support for other languages too.

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
11. [The theme is spacemacs-theme](https://github.com/nashamri/spacemacs-theme)
12. [Color-identifiers-Mode](https://github.com/ankurdave/color-identifiers-mode)
13. [Origami](https://github.com/gregsexton/origami.el)
14. [Git-Gutter](https://github.com/syohex/emacs-git-gutter)
15. [Markdown-mode](https://jblevins.org/projects/markdown-mode/)(Requires pandoc to run)
16. [Magit](https://github.com/magit/magit)
17. [Flycheck](https://github.com/flycheck/flycheck)
18. [use-package](https://github.com/jwiegley/use-package)
19. [ethan-wspace](https://github.com/glasserc/ethan-wspace)
20. [elpy](https://github.com/jorgenschaefer/elpy)
21. [Flycheck-Pos-Tip](https://github.com/flycheck/flycheck-pos-tip)

## Notes
* I am not adding my custom macros because it is easier to understand if you open emacs config and see them alone.
That way you can change them to your liking.
* Repo will be updated everytime i add a plugin to my configuration.
* I am searching for a good refactoring tool (C/C++) for the setup to be complete
  after that i dont think any plugins will be needed.
