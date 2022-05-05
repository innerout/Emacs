#!/bin/bash
set -xe

distro=$(grep "^ID=" /etc/os-release | cut -d\= -f2 | sed -e 's/"//g')

case $distro in
	"ubuntu" | "debian")
		sudo apt install emacs npm yarn python3-pip gcc clang fd-find ripgrep -y
		wget https://github.com/latex-lsp/texlab/releases/download/v3.3.2/texlab-x86_64-linux.tar.gz
		tar -xf texlab-x86_64-linux.tar.gz
		sudo mv texlab /usr/bin/texlab
		sudo ln -sf /usr/bin/texlab /bin/texlab
		sudo mkdir -p /usr/share/fonts/truetype/ttf-monaco
		sudo wget https://gist.github.com/rogerleite/b50866eb7f7b5950da01ae8927c5bd61/raw/862b6c9437f534d5899e4e68d60f9bf22f356312/mfont.ttf -O - >/usr/share/fonts/truetype/ttf-monaco/Monaco_Linux.ttf
		sudo fc-cache
		;;
	"fedora")
		sudo dnf install emacs npm yarn python3-pip gcc clang fd-find ripgrep -y
		wget https://github.com/latex-lsp/texlab/releases/download/v3.3.2/texlab-x86_64-linux.tar.gz
		tar -xf texlab-x86_64-linux.tar.gz
		sudo mv texlab /usr/bin/texlab
		sudo ln -sf /usr/bin/texlab /bin/texlab
		sudo mkdir -p /usr/share/fonts/truetype/ttf-monaco
		sudo wget https://gist.github.com/rogerleite/b50866eb7f7b5950da01ae8927c5bd61/raw/862b6c9437f534d5899e4e68d60f9bf22f356312/mfont.ttf -O - >/usr/share/fonts/truetype/ttf-monaco/Monaco_Linux.ttf
		sudo fc-cache
		;;
	"arch" | "manjaro")
		yay -S emacs-native-comp texlab npm python-pip clang ttf-monaco fd ripgrep --noconfirm
		yay -S libvoikko nuspell hspell hunspell aspell aspell-en hunspell-en_us
		;;
	*)
		echo "\"$distro\" is not supported distro, so please install packages manually."
		;;
esac

pip install "python-lsp-server[all]" pyls-flake8 pylsp-mypy pyls-isort python-lsp-black pyls-memestra pylsp-rope --user --no-input
pip install cmake-language-server grip --user --no-input
sudo npm i -g --silent yaml-language-server bash-language-server
