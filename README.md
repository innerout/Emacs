# Requirements
* Emacs configuration tested on the master branch on savannah.

### Arch

#### ```sudo yay -S emacs-git```

## Installation

``` bash
1. git clone https://github.com/innerout/Emacs.git
2. cd Emacs
3. cp init.el ~/.emacs.d/init.el
```

## Notes

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

## Cmake Language Server

__Dependencies__ : pip install cmake-language-server --user

## Bash Language Server

__Dependencies__ : npm i -g bash-language-server

## Markdown-mode

__Dependencies__ : [pandoc](https://pandoc.org/) [grip](https://github.com/joeyespo/grip)

## All-the-icons

M-x all-the-icons-install-fonts
