# Requirements
* Emacs config tested on the version available on Arch linux.

### Arch

#### ```sudo pacman -S emacs```

## Installation

If you have reached here you got emacs installed by now.The only thing you have to do is to run these 4 commands and you will have the whole configuration running.

``` bash
1. git clone https://github.com/innerout/Emacs.git
2. cd Emacs
3. cp init.el ~/.emacs.d/init.el
4. cp -r notinmelpa ~/.emacs.d/
```
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
12. [Magit](https://github.com/magit/magit)
13. [Flycheck](https://github.com/flycheck/flycheck)
14. [use-package](https://github.com/jwiegley/use-package)
15. [ethan-wspace](https://github.com/glasserc/ethan-wspace)
16. [Flycheck-Pos-Tip](https://github.com/flycheck/flycheck-pos-tip)
17. [company](https://github.com/company-mode/company-mode)
18. [rainbow-delimiters](https://github.com/Fanael/rainbow-delimiters)
19. [dashboard](https://github.com/rakanalh/emacs-dashboard)
20. [org-mode](https://orgmode.org/)
21. [beacon](https://github.com/Malabarba/beacon)
22. [academic-phrases](https://github.com/nashamri/academic-phrases)
23. [org-bullets](https://github.com/sabof/org-bullets)
24. [ccls-emacs](https://github.com/MaskRay/emacs-ccls)
25. [company-lsp](https://github.com/tigersoldier/company-lsp)
26. [lsp-ui](https://github.com/emacs-lsp/lsp-ui)
27. [helm-themes](https://github.com/syohex/emacs-helm-themes)
28. [lsp-python](https://github.com/emacs-lsp/lsp-python)
29. [all-the-icons](https://github.com/domtronn/all-the-icons.el)
30. [which-key](https://github.com/justbur/emacs-which-key)
31. [bug-hunter](https://github.com/Malabarba/elisp-bug-hunter)
32. [mu4e-alert](https://github.com/iqbalansari/mu4e-alert)
33. [mu4e-conversation](https://gitlab.com/ambrevar/mu4e-conversation)
34. [pdf-tools](https://github.com/politza/pdf-tools)
35. [langtool](https://github.com/mhayashi1120/Emacs-langtool)
36. [multiple-cursors](https://github.com/magnars/multiple-cursors.el)
37. [elfeed](https://github.com/skeeto/elfeed)
38. [sublimity](https://github.com/zk-phi/sublimity)
39. [rmsbolt](https://gitlab.com/jgkamat/rmsbolt)
40. [doom-modeline](https://github.com/seagle0128/doom-modeline)
41. [gcmh](https://gitlab.com/koral/gcmh)
42. [winum](https://github.com/deb0ch/emacs-winum)
43. [org-fancy-priorities](https://github.com/harrybournis/org-fancy-priorities)
44. [grip](https://github.com/seagle0128/grip-mode)
45. [messages-are-flowing](https://github.com/legoscia/messages-are-flowing)
46. [writegood-mode](https://github.com/bnbeckwith/writegood-mode/tree/master)
47. [persistent-scratch](https://github.com/Fanael/persistent-scratch/tree/master)
48. [drag-stuff](https://github.com/rejeep/drag-stuff.el/tree/master)
49. [toc-org](https://github.com/snosov1/toc-org)
50. [emacs-emojify](https://github.com/iqbalansari/emacs-emojify)
51. [clang-format](https://github.com/emacsmirror/clang-format/tree/master)
52. [helm-ag](https://github.com/syohex/emacs-helm-ag/tree/master)
53. [wgrep](https://github.com/mhayashi1120/Emacs-wgrep/tree/master)

## Notes
* I am not adding my custom macros because it is easier to understand if you open init.el and see them yourself.
That way you can change them to your liking.
* Font __Adobe Source Code Pro__ to install run __sudo pacman -S adobe-source-code-pro-fonts__

# HOWTO SETUP PLUGINS

## Ccls

__REQUIREMENTS__ = __trizen bear-git__ __pip install git+https://github.com/Sarcasm/compdb.git#egg=compdb__

### To install ccls use archlinuxcn repo

sudo pacman -S ccls-git

### To use in a project

```bash

cd MakefileDirectory
touch .ccls-root
bear make
cd ..
compdb -p MakefileDirectory/ list > compile_commands.json
mv compile_commands.json  MakefileDirectory/

```
## Cscope

__REQUIREMENTS__ = __sudo pacman -S cscope__

Set initial directory (root folder of project) C-c s a

Create list of file to index C-c s L

Create list and index C-c s I

## Markdown-mode

__REQUIREMENTS__ = __sudo pacman -S pandoc__

## All-the-icons

M-x all-the-icons-install-fonts
