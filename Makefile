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
	@echo "setup-ci - clone emacs-ci to run project CI actions such as linting"
	@echo "bootstrap - install Straight.el"
	@echo "install - install package dependencies"
	@echo "build - byte compile the package"
	@echo "lint - check style with package-lint"
	@echo "checkdoc - check docstrings"
	@echo
	@echo "**All of these actions take effect and are contained inside the emacs-ci/ folder --- they do not affect the system Emacs configuration.**"

setup-ci:
	@if [ -d ".emacs-ci" ]; then \
		echo "--> Updating existing emacs-ci repository..."; \
		cd .emacs-ci && git pull; \
	else \
		echo "--> Cloning emacs-ci repository..."; \
		git clone https://github.com/countvajhula/emacs-ci.git .emacs-ci; \
	fi

clean:
	cd .emacs-ci && rm -rf ci-init

bootstrap:
	cd .emacs-ci && emacs --batch --quick --load bootstrap.el

install:
	cd .emacs-ci && emacs --batch --quick --load install.el

build:
	cd .emacs-ci && emacs --batch --quick --load build.el

lint:
	cd .emacs-ci && emacs --batch --quick --load lint.el

checkdoc:
	cd .emacs-ci && emacs --batch --quick --load checkdoc.el

.PHONY: help setup-ci clean bootstrap install build lint checkdoc
