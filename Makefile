# Determine this makefile's path.
# Be sure to place this BEFORE `include` directives, if any.
# Source: https://stackoverflow.com/a/27132934/323874
THIS_FILE := $(lastword $(MAKEFILE_LIST))

EMACS=emacs

export CI_PACKAGES=rigpa

help:
	@echo "Run common development actions."
	@echo
	@echo "    Usage: make <target>"
	@echo "    where <target> is one of:"
	@echo
	@echo "help - show this menu"
	@echo "clean - remove all build artifacts"
	@echo "setup-ci - clone elci to run project CI actions such as linting"
	@echo "bootstrap - install Straight.el"
	@echo "install - install package dependencies"
	@echo "byte-compile - byte compile the package"
	@echo "native-compile - native compile the package"
	@echo "lint - check style with package-lint"
	@echo "checkdoc - check docstrings"
	@echo
	@echo "**All of these actions take effect and are contained inside the elci/ folder --- they do not affect the system Emacs configuration.**"

setup-ci:
	@if [ -d ".elci" ]; then \
		echo "--> Updating existing elci repository..."; \
		cd .elci && git pull; \
	else \
		echo "--> Cloning elci repository..."; \
		git clone https://github.com/countvajhula/elci.git .elci; \
	fi

clean:
	cd .elci && rm -rf init

bootstrap:
	cd .elci && emacs --batch --quick --load bootstrap.el

install:
	cd .elci && emacs --batch --quick --load install.el

byte-compile:
	cd .elci && emacs --batch --quick --load byte-compile.el

native-compile:
	cd .elci && emacs --batch --quick --load native-compile.el

lint:
	cd .elci && emacs --batch --quick --load lint.el

checkdoc:
	cd .elci && emacs --batch --quick --load checkdoc.el

.PHONY: help setup-ci clean bootstrap install byte-compile native-compile lint checkdoc
