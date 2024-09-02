emacs_path := ~/.emacs.d
bin_path := $(emacs_path)/bin
cmd := $(bin_path)/doom
sync:
	$(cmd) sync

doctor:
	$(cmd) doctor

upgrade:
	$(cmd) upgrade

upgrade-manual:
	cd $(emacs_path) && git pull && $(cmd) sync -u

build:
	$(cmd) build

clean:
	find $(emacs_path) -name "*.elc" -type f -delete

clear-package-cache:
	rm -rf $(emacs_path)/.local/straight
	$(cmd) sync
