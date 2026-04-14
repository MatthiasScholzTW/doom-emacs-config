# Doom Emacs configuration

## Emacs Plus (Cask)

This setup uses [emacs-plus](https://github.com/d12frosted/homebrew-emacs-plus)
installed via Homebrew cask (`emacs-plus-app`). It provides Emacs 30 with
native compilation, tree-sitter, and full daemon + GUI support out of the box.

### Prerequisites

```sh
brew tap d12frosted/emacs-plus
brew install --cask emacs-plus-app
```

The cask installs both **Emacs.app** and **Emacs Client.app** to `/Applications/`.
PATH is injected at install time — Nix, Homebrew, and direnv paths are
available without `exec-path-from-shell` or shell wrappers.

## Emacs Daemon & Emacs Client.app

Emacs runs as a background daemon managed by macOS `launchd`. All frames
connect to the same long-running process, so buffers, LSP sessions, and
agent-shell sessions are always available — no matter how many windows
you open.

**Emacs Client.app** (bundled with emacs-plus) opens new GUI frames via
`emacsclient`. It can be launched from Spotlight, the Dock, or Finder.
It also handles Finder "Open With", drag-and-drop, and `org-protocol://`
URLs.

### Getting Started

1. Quit any running Emacs instance.
2. Install and start the launchd service:
   ```sh
   cd ~/.doom.d
   make install-daemon
   make start-daemon
   ```
3. Open **Emacs Client.app** from Spotlight or `/Applications/`.
4. Press `SPC m a w` to open the Agents workspace.
5. After any `doom sync` or config change, restart with:
   ```sh
   make restart-daemon
   # or combine both steps:
   make sync-restart
   ```

### Components

- **`bin/emacs-daemon`** — Starts, stops, or restarts the daemon.
- **`bin/restart-daemon`** — Restarts the daemon via launchd. Use after
  `doom sync` or config changes.
- **`~/Library/LaunchAgents/org.gnu.emacs.daemon.plist`** — launchd
  service that starts the daemon on login and restarts on crash.

### Agent Workspace

All agent-shell sessions are grouped in a single Doom workspace called
**Agents**. This keeps them separated from your code buffers and lets
you switch back and forth. Any ACP-supported agent (Auggie, OpenCode,
Gemini, etc.) runs inside this workspace.

| Keybinding  | Action                                     |
|-------------|--------------------------------------------|
| `SPC m a w` | Switch to (or create) the Agents workspace |
| `SPC TAB l` | Restore a saved workspace after restart    |

Doom's `persp-mode` persists workspaces across daemon restarts, so your
agent session layout is restored when you reconnect with Emacs Client.app.

### Makefile Targets

```
make install-daemon   # Register the launchd service
make start-daemon     # Start the daemon now
make stop-daemon      # Stop the daemon
make restart-daemon   # Full restart via launchd
make sync-restart     # doom sync, then restart daemon
make upgrade-restart  # doom upgrade, then restart daemon
make uninstall-daemon # Remove the launchd service
```

### Logs

Daemon stdout and stderr are written to:

- `/tmp/emacs-daemon.stdout.log`
- `/tmp/emacs-daemon.stderr.log`

Check these if the daemon fails to start.

## References

- [Doomemacs](https://github.com/doomemacs/doomemacs)
- [emacs-plus](https://github.com/d12frosted/homebrew-emacs-plus)
- [Learning via YouTube](https://www.youtube.com/playlist?list=PLhXZp00uXBk4np17N39WvB80zgxlZfVwj)
