# Requirements
* Emacs config tested on the version available on Arch linux.

### Arch

#### ```sudo pacman -S emacs```

## Installation

``` bash
1. git clone https://github.com/innerout/Emacs.git
2. cd Emacs
3. cp init.el ~/.emacs.d/init.el
```
## Installed plugins

1. [aggressive-indent](https://github.com/Malabarba/aggressive-indent-mode)
2. [neotree](https://github.com/jaypei/emacs-neotree)
3. [helm](https://github.com/emacs-helm/helm)
4. [smartparens](https://github.com/Fuco1/smartparens)
5. [highlight-indent-guides](https://github.com/DarthFennec/highlight-indent-guides)
6. [yasnippet](https://github.com/joaotavora/yasnippet)
7. [The theme is spacemacs-theme](https://github.com/nashamri/spacemacs-theme)
8. [Color-identifiers-Mode](https://github.com/ankurdave/color-identifiers-mode)
9. [hs-minor-mode](https://www.emacswiki.org/emacs/HideShow)
10. [Git-Gutter](https://github.com/syohex/emacs-git-gutter)
11. [Magit](https://github.com/magit/magit)
12. [Flycheck](https://github.com/flycheck/flycheck)
13. [use-package](https://github.com/jwiegley/use-package)
14. [ethan-wspace](https://github.com/glasserc/ethan-wspace)
15. [Flycheck-Pos-Tip](https://github.com/flycheck/flycheck-pos-tip)
16. [company](https://github.com/company-mode/company-mode)
17. [rainbow-delimiters](https://github.com/Fanael/rainbow-delimiters)
18. [dashboard](https://github.com/rakanalh/emacs-dashboard)
19. [org-mode](https://orgmode.org/)
20. [beacon](https://github.com/Malabarba/beacon)
21. [academic-phrases](https://github.com/nashamri/academic-phrases)
22. [org-bullets](https://github.com/sabof/org-bullets)
23. [ccls-emacs](https://github.com/MaskRay/emacs-ccls)
24. [company-lsp](https://github.com/tigersoldier/company-lsp)
25. [lsp-ui](https://github.com/emacs-lsp/lsp-ui)
26. [helm-themes](https://github.com/syohex/emacs-helm-themes)
27. [lsp-python](https://github.com/emacs-lsp/lsp-python)
28. [all-the-icons](https://github.com/domtronn/all-the-icons.el)
29. [which-key](https://github.com/justbur/emacs-which-key)
30. [bug-hunter](https://github.com/Malabarba/elisp-bug-hunter)
31. [mu4e-alert](https://github.com/iqbalansari/mu4e-alert)
32. [pdf-tools](https://github.com/politza/pdf-tools)
33. [langtool](https://github.com/mhayashi1120/Emacs-langtool)
34. [multiple-cursors](https://github.com/magnars/multiple-cursors.el)
35. [elfeed](https://github.com/skeeto/elfeed)
36. [rmsbolt](https://gitlab.com/jgkamat/rmsbolt)
37. [doom-modeline](https://github.com/seagle0128/doom-modeline)
38. [gcmh](https://gitlab.com/koral/gcmh)
39. [winum](https://github.com/deb0ch/emacs-winum)
40. [org-fancy-priorities](https://github.com/harrybournis/org-fancy-priorities)
41. [grip](https://github.com/seagle0128/grip-mode)
42. [messages-are-flowing](https://github.com/legoscia/messages-are-flowing)
43. [writegood-mode](https://github.com/bnbeckwith/writegood-mode/tree/master)
44. [persistent-scratch](https://github.com/Fanael/persistent-scratch/tree/master)
45. [drag-stuff](https://github.com/rejeep/drag-stuff.el/tree/master)
46. [toc-org](https://github.com/snosov1/toc-org)
47. [emacs-emojify](https://github.com/iqbalansari/emacs-emojify)
48. [clang-format](https://github.com/emacsmirror/clang-format/tree/master)
49. [helm-ag](https://github.com/syohex/emacs-helm-ag/tree/master)
50. [wgrep](https://github.com/mhayashi1120/Emacs-wgrep/tree/master)
51. [mu4e-maildirs](https://github.com/agpchil/mu4e-maildirs-extension)
52. [cmake-mode](https://melpa.org/packages/cmake-mode-20190710.1319.el)
53. [cmake-font-lock](https://github.com/Lindydancer/cmake-font-lock/tree/master)
54. [eldoc-cmake](https://github.com/ikirill/eldoc-cmake/tree/master)

## Notes
* I am not adding my custom macros because it is easier to understand if you open init.el and see them yourself.
That way you can change them to your liking.
* Font [Adobe Source Code Pro](https://www.archlinux.org/packages/extra/any/adobe-source-code-pro-fonts/)

# HOWTO SETUP PLUGINS

## Ccls

__Dependencies__ :  [bear](https://github.com/rizsotto/Bear) [compdb](https://github.com/Sarcasm/compdb)
### To install ccls use [archlinuxcn](https://github.com/archlinuxcn/repo) repo

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

## Markdown-mode

__Dependencies__ : [pandoc](https://pandoc.org/)

## All-the-icons

M-x all-the-icons-install-fonts
