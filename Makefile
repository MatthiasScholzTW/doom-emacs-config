bin_path := ~/.emacs.d.doomemacs/bin/
cmd := $(bin_path)/doom
sync:
	$(cmd) sync

doctor:
	$(cmd) doctor

upgrade:
	$(cmd) upgrade