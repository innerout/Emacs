# Requirements
* Emacs based on the version of Arch linux.

### Arch

#### ```sudo pacman -S emacs```

### Ubuntu/Debian

#### ```sudo add-apt-repository ppa:kelleyk/emacs```
#### ```sudo apt-get update```
#### ```sudo apt-get install emacs26```

### Fedora

#### ```sudo dnf install emacs```

## Installation

If you have reached here you got emacs installed by now.The only thing you have to do is to run these 3 commands and you will have the whole configuration running.

1. git clone https://github.com/innerout/Emacs.git
2. cd Emacs
3. cp init.el ~/.emacs.d/init.el

## Installed plugins
1. [aggressive-indent](https://github.com/Malabarba/aggressive-indent-mode)
2. [neotree](https://github.com/jaypei/emacs-neotree)
3. [helm](https://github.com/emacs-helm/helm)
4. [smartparens](https://github.com/Fuco1/smartparens)
5. [xcscope](https://github.com/dkogan/xcscope.el)
6. [highlight-indent-guides](https://github.com/DarthFennec/highlight-indent-guides)
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
25. [ccls-emacs](https://github.com/MaskRay/emacs-ccls)
26. [company-lsp](https://github.com/tigersoldier/company-lsp)
27. [lsp-ui](https://github.com/emacs-lsp/lsp-ui)
28. [helm-themes](https://github.com/syohex/emacs-helm-themes)
29. [lsp-python](https://github.com/emacs-lsp/lsp-python)
30. [all-the-icons](https://github.com/domtronn/all-the-icons.el)
31. [which-key](https://github.com/justbur/emacs-which-key)
32. [bug-hunter](https://github.com/Malabarba/elisp-bug-hunter)
33. [mu4e-alert](https://github.com/iqbalansari/mu4e-alert)
34. [mu4e-conversation](https://gitlab.com/ambrevar/mu4e-conversation)
35. [pdf-tools](https://github.com/politza/pdf-tools)
36. [langtool](https://github.com/mhayashi1120/Emacs-langtool)
37. [multiple-cursors](https://github.com/magnars/multiple-cursors.el)
38. [elfeed](https://github.com/skeeto/elfeed)
39. [sublimity](https://github.com/zk-phi/sublimity)
40. [rmsbolt](https://gitlab.com/jgkamat/rmsbolt)
41. [doom-modeline](https://github.com/seagle0128/doom-modeline)
42. [gcmh](https://gitlab.com/koral/gcmh)

## Notes
* I am not adding my custom macros because it is easier to understand if you open emacs config and see them alone.
That way you can change them to your liking.
* Repo will be updated everytime i add a plugin to my configuration.
* Font __adobe-source-code-pro__ sudo pacman -S adobe-source-code-pro-fonts

# HOWTO SETUP PLUGINS

## Ccls

__REQUIREMENTS__ = __trizen bear-git__ __pip install git+https://github.com/Sarcasm/compdb.git#egg=compdb__

### To install ccls use archlinuxcn repo

sudo pacman -S ccls-git

### To use in a project

Run the root Makefile with bear.

bear make

#cd one directory up and run the command below

compdb -p build/ list > compile_commands.json

touch .ccls-root #in the root folder

mv compile_commands.json inside the folder

## Cscope

__REQUIREMENTS__ = __sudo pacman -S cscope__

Set initial directory (root folder of project) C-c s a

Create list of file to index C-c s L

Create list and index C-c s I

## Markdown-mode

__REQUIREMENTS__ = __sudo pacman -S pandoc__

## All-the-icons

M-x all-the-icons-install-fonts
