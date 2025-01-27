SHELL := bash
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:

DOOM_CONFIG_FILES := config.el custom.el +definitions.el +general.el init.el +keys.el +libraries.el packages.el
EMACS_CONFIG_FILES := general.el keys.el libraries.el misc.el org.el .emacs

doom_files := $(DOOM_CONFIG_FILES)
emacs_files := $(EMACS_CONFIG_FILES)

.PHONY: all install clean

all: install

install: install-doom install-emacs install-tmux install-vim install-xmodmap install-zshell

install-doom:
	mkdir -p ~/.doom.d
	$(foreach file,$(doom_files),\
		rm -f ~/.doom.d/$(file); \
		ln -s $(PWD)/doom/$(file) ~/.doom.d/$(file);)

install-emacs:
	mkdir -p ~/etc/emacs
	$(foreach file,$(emacs_files),\
		rm -f ~/etc/emacs/$(file); \
		ln -s $(PWD)/emacs/$(file) ~/etc/emacs/$(file);)

install-tmux:
	rm -f ~/.tmux.conf
	ln -s $(PWD)/tmux/.tmux.conf ~/.tmux.conf

install-vim:
	rm -f ~/.vimrc
	ln -s $(PWD)/vim/.vimrc ~/.vimrc

install-xmodmap:
	rm -f ~/.Xmodmap
	ln -s $(PWD)/xmodmap/.Xmodmap ~/.Xmodmap

install-zshell:
	rm -f ~/.zshenv
	ln -s $(PWD)/zsh/.zshenv ~/.zshenv
