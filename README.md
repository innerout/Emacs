# Requirements
* My Emacs configuration is based on the emacs-pgtk-native-comp-git AUR package.

### Arch

To run my configuration you need to run the following commands in ArchLinux.
#### `yay -S emacs-pgtk-native-comp-git texlab npm python-pip clang ttf-monaco`
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

## Markdown-mode

__Dependencies__ : [pandoc](https://pandoc.org/) [grip](https://github.com/joeyespo/grip)
