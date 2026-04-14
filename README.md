# Doom Emacs configuration

## Emacs Daemon & EmacsClient Setup

Emacs runs as a background daemon managed by macOS `launchd`. All frames connect to the same
long-running process, so buffers, LSP sessions, and agent-shell sessions (Auggie, OpenCode, Gemini,
etc.) are always available — no matter how many windows you open.

### Getting Started

1. Quit your current Emacs.app instance.
2. Install and start the launchd service:
   ```sh
   cd ~/.doom.d
   make install-daemon
   make start-daemon
   ```
3. Open **EmacsClient.app** from `~/Applications/` (or use Spotlight → "EmacsClient").
4. Press `SPC m a w` to open the Agents workspace.
5. After any `doom sync` or config change, restart with:
   ```sh
   make restart-daemon
   # or combine both steps:
   make sync-restart
   ```

### Why a daemon?

macOS GUI apps (including Emacs.app) launch in a minimal environment that is missing Nix, Homebrew,
and direnv paths. The daemon starts through a wrapper script (`bin/emacs-daemon`) that sources the
full zsh login environment before running `emacs --daemon`. This ensures every tool on your normal
`PATH` is visible inside Emacs.

### Components

- **`bin/emacs-daemon`** — Sources the shell env and starts/stops/restarts the daemon.
- **`bin/restart-daemon`** — Restarts the daemon via launchd. Use after `doom sync` or config
  changes.
- **`~/Library/LaunchAgents/org.gnu.emacs.daemon.plist`** — launchd service that starts the daemon
  on login and restarts on crash.
- **`~/Applications/EmacsClient.app`** — Clickable macOS app that opens a new GUI frame via
  `emacsclient -c`.

### EmacsClient.app

A minimal macOS app bundle that lives in `~/Applications/`. It can be launched from Spotlight, the
Dock, or Finder — no terminal required. It runs `emacsclient -c -n` to open a new GUI frame. If
the daemon is not running yet, it starts it automatically before connecting.

The app reuses the Emacs icon from `/Applications/Emacs.app`.

### Agent Workspace

All agent-shell sessions are grouped in a single Doom workspace called **Agents**. This keeps them
separated from your code buffers and lets you switch back and forth. Any ACP-supported agent
(OpenCode, Gemini, etc.) runs inside this workspace.

| Keybinding    | Action                                      |
|---------------|---------------------------------------------|
| `SPC m a w`   | Switch to (or create) the Agents workspace  |
| `SPC TAB l`   | Restore a saved workspace after restart     |

Doom's `persp-mode` persists workspaces across daemon restarts, so your agent session layout is
restored when you reconnect with EmacsClient.app.

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
- [Learning via YouTube](https://www.youtube.com/playlist?list=PLhXZp00uXBk4np17N39WvB80zgxlZfVwj)
