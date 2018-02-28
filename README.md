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
2. [neotree](https://github.com/jaypei/emacs-neotree)
3. [helm](https://github.com/emacs-helm/helm)
4. [autopair](https://github.com/joaotavora/autopair)
5. [xcscope](https://github.com/dkogan/xcscope.el)
6. [indent-guide](https://github.com/zk-phi/indent-guide)
7. [yasnippet](https://github.com/joaotavora/yasnippet)
8. [autocomplete](https://github.com/auto-complete/auto-complete)
9. [autocomplete-c-headers](https://github.com/mooz/auto-complete-c-headers)
10. [The theme is spacemacs-theme](https://github.com/nashamri/spacemacs-theme)
11. [Color-identifiers-Mode](https://github.com/ankurdave/color-identifiers-mode)
12. [hs-minor-mode](https://www.emacswiki.org/emacs/HideShow)
13. [Git-Gutter](https://github.com/syohex/emacs-git-gutter)
14. [Markdown-mode](https://jblevins.org/projects/markdown-mode/)(Requires pandoc to run)
15. [Magit](https://github.com/magit/magit)
16. [Flycheck](https://github.com/flycheck/flycheck)
17. [use-package](https://github.com/jwiegley/use-package)
18. [ethan-wspace](https://github.com/glasserc/ethan-wspace)
19. [elpy](https://github.com/jorgenschaefer/elpy)
20. [Flycheck-Pos-Tip](https://github.com/flycheck/flycheck-pos-tip)
21. [company](https://github.com/company-mode/company-mode)
22. [irony](https://github.com/Sarcasm/irony-mode)
23. [company-irony](https://github.com/Sarcasm/company-irony)
24. [company-c-headers](https://github.com/randomphrase/company-c-headers)
25. [rainbow-delimiters](https://github.com/Fanael/rainbow-delimiters)
26. [dashboard](https://github.com/rakanalh/emacs-dashboard)
27. [org-mode](https://orgmode.org/)
## Notes
* I am not adding my custom macros because it is easier to understand if you open emacs config and see them alone.
That way you can change them to your liking.
* Repo will be updated everytime i add a plugin to my configuration.
* I am searching for a good refactoring tool (C/C++) for the setup to be complete
  after that i dont think any plugins will be needed.
* Font __adobe-source-code-pro__ sudo pacman -S adobe-source-code-pro-fonts

# HOWTO SETUP PLUGINS

## IRONY

__REQUIREMENTS__ = __sudo pacman -S cmake clang__ __sudo dnf install cmake clang clang-devel llvm clang-libs__

Everytime irony or company-irony is updated you have to run M-x irony-install-server.

make CC='cc_args.py gcc' -B

## Cscope

__REQUIREMENTS__ = __sudo pacman -S cscope__

Set initial directory (root folder of project) C-c s a

Create list of file to index C-c s L

Create list and index C-c s I

## Markdown-mode

__REQUIREMENTS__ = __sudo pacman -S pandoc__
