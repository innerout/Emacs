# Requirements
* Emacs based on the version of Arch linux.

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
4. [smartparens](https://github.com/Fuco1/smartparens)
5. [xcscope](https://github.com/dkogan/xcscope.el)
6. [indent-guide](https://github.com/zk-phi/indent-guide)
7. [yasnippet](https://github.com/joaotavora/yasnippet)
8. [The theme is spacemacs-theme](https://github.com/nashamri/spacemacs-theme)
9. [Color-identifiers-Mode](https://github.com/ankurdave/color-identifiers-mode)
10. [hs-minor-mode](https://www.emacswiki.org/emacs/HideShow)
11. [Git-Gutter](https://github.com/syohex/emacs-git-gutter)
12. [Markdown-mode](https://jblevins.org/projects/markdown-mode/)(Requires pandoc to run)
13. [Magit](https://github.com/magit/magit)
14. [Flycheck](https://github.com/flycheck/flycheck)
15. [use-package](https://github.com/jwiegley/use-package)
16. [ethan-wspace](https://github.com/glasserc/ethan-wspace)
17. [Flycheck-Pos-Tip](https://github.com/flycheck/flycheck-pos-tip)
18. [company](https://github.com/company-mode/company-mode)
19. [rainbow-delimiters](https://github.com/Fanael/rainbow-delimiters)
20. [dashboard](https://github.com/rakanalh/emacs-dashboard)
21. [org-mode](https://orgmode.org/)
22. [beacon](https://github.com/Malabarba/beacon)
23. [academic-phrases](https://github.com/nashamri/academic-phrases)
24. [org-bullets](https://github.com/sabof/org-bullets)
25. [cquery-emacs](https://github.com/cquery-project/emacs-cquery)
26. [company-lsp](https://github.com/tigersoldier/company-lsp)
27. [lsp-ui](https://github.com/emacs-lsp/lsp-ui)
28. [helm-themes](https://github.com/syohex/emacs-helm-themes)
29. [lsp-python](https://github.com/emacs-lsp/lsp-python)

## Notes
* I am not adding my custom macros because it is easier to understand if you open emacs config and see them alone.
That way you can change them to your liking.
* Repo will be updated everytime i add a plugin to my configuration.
* Font __adobe-source-code-pro__ sudo pacman -S adobe-source-code-pro-fonts

# HOWTO SETUP PLUGINS

## Cquery

__REQUIREMENTS__ = __yaourt bear-git cquery__ __pip install git+https://github.com/Sarcasm/compdb.git#egg=compdb__

Run in the folder with the main Makefile that builds everything.

bear make

cd one directory up and run the command below

compdb -p build/ list > compile_commands.json

mv compile_commands.json inside the folder

## lsp-python

__REQUIREMENTS__ = __sudo pip install 'python-language-server[all]' pyls-mypy pyls-isort__

## Cscope

__REQUIREMENTS__ = __sudo pacman -S cscope__

Set initial directory (root folder of project) C-c s a

Create list of file to index C-c s L

Create list and index C-c s I

## Markdown-mode

__REQUIREMENTS__ = __sudo pacman -S pandoc__
