emacs_path := ~/.emacs.d
bin_path := $(emacs_path)/bin
# Use emacs-plus (cask) binary so doom builds for Emacs 30
export EMACS := /Applications/Emacs.app/Contents/MacOS/bin/emacs
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

emacs_version := 30.2
emacs_vterm_dir := $(emacs_path)/.local/straight/build-$(emacs_version)/vterm/build

libvterm_version := 0.3.3
# FIXME depends on brew, support nix, too
libvterm_dir := /opt/homebrew/Cellar/libvterm/$(libvterm_version)
setup-vterm:
	brew install libvterm

build-vterm:
	mkdir -p $(emacs_vterm_dir)
	cd $(emacs_vterm_dir) && cmake -DLIBVTERM_INCLUDE_DIR=$(libvterm_dir)/include -DLIBVTERM_LIBRARY=$(libvterm_dir)/lib/libvterm.0.dylib ..
	cd $(emacs_vterm_dir) && make

clean:
	find $(emacs_path) -name "*.elc" -type f -delete

clear-package-cache:
	rm -rf $(emacs_path)/.local/straight
	$(cmd) sync


# ── Emacs Daemon (launchd) ──────────────────────────────────────────────

daemon_plist_label := org.gnu.emacs.daemon
daemon_plist := ~/Library/LaunchAgents/$(daemon_plist_label).plist
daemon_script := $(CURDIR)/bin/emacs-daemon

install-daemon: ## Load the launchd service (starts Emacs daemon on login)
	launchctl bootstrap gui/$$(id -u) $(daemon_plist)
	@echo "Emacs daemon service installed. It will start on next login."
	@echo "Run 'make start-daemon' to start it now."

uninstall-daemon: stop-daemon ## Remove the launchd service
	launchctl bootout gui/$$(id -u) $(daemon_plist) 2>/dev/null || true
	@echo "Emacs daemon service uninstalled."

start-daemon: ## Start the Emacs daemon
	$(daemon_script) start

stop-daemon: ## Stop the Emacs daemon
	$(daemon_script) stop

status-daemon: ## Check if the Emacs daemon is running
	$(daemon_script) status

restart-daemon: ## Restart daemon (use after doom sync / upgrade)
	$(CURDIR)/bin/restart-daemon

sync-restart: sync restart-daemon ## doom sync then restart daemon

upgrade-restart: upgrade restart-daemon ## doom upgrade then restart daemon