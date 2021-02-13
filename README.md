# Requirements
* My Emacs configuration is based on the emacs-pgtk-native-comp-git AUR package.

### Arch

To run my configuration you need to run the following commands in ArchLinux.
#### `yay -S emacs-pgtk-native-comp-git texlab ccls-git npm python-pip clang-format ttf-monaco`
#### `pip install pyright cmake-language-server grip --user`
#### `sudo npm i -g yaml-language-server bash-language-server`

## Installation

``` bash
1. git clone https://github.com/innerout/Emacs.git
2. cp -r Emacs/.doom.d ~/
3. cd ~/
4. git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
5. ~/.emacs.d/bin/doom install
```

## Ccls

If you don't develop a cmake project you may need the tools below.

__Dependencies__ :  [bear](https://github.com/rizsotto/Bear) [compdb](https://github.com/Sarcasm/compdb)

### To use in a project

```bash

cd MakefileDirectory
bear make
cd ..
compdb -p MakefileDirectory/ list > compile_commands.json
mv compile_commands.json  MakefileDirectory/

```

## Markdown-mode

__Dependencies__ : [pandoc](https://pandoc.org/) [grip](https://github.com/joeyespo/grip)
