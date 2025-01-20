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

emacs_version := 30.0.92
emacs_vterm_dir := /Users/matthias/.emacs.d/.local/straight/build-$(emacs_version)/vterm/build
libvterm_version := 0.3.3
libvterm_dir := /opt/homebrew/Cellar/libvterm/$(libvterm_version)
build-vterm:
	mkdir $(emacs_vterm_dir)
	cd $(emacs_vterm_dir) && cmake -DLIBVTERM_INCLUDE_DIR=$(libvterm_dir)/include -DLIBVTERM_LIBRARY=$(libvterm_dir)/lib/libvterm.0.dylib ..
	cd $(emacs_vterm_dir) && make

clean:
	find $(emacs_path) -name "*.elc" -type f -delete

clear-package-cache:
	rm -rf $(emacs_path)/.local/straight
	$(cmd) sync
