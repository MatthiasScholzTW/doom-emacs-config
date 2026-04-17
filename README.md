# My Doom Emacs Configuration

This repository contains my personal configuration for [Doom Emacs](https://github.com/doomemacs/doomemacs). It is tailored for my development workflow, focusing on Go, Python, Nix, and various DevOps tools.

## Features

- **Modern UI**: Utilizes `vertico` for completion, `treemacs` for file navigation, and the `doom-one` theme.
- **Language Support**: Comprehensive support for several languages including:
  - Go (`gopls`)
  - Python (`pyright`)
  - Nix
  - Terraform / HCL (with Terramate LSP)
  - Shell scripting
  - YAML, JSON, Markdown
  - OPA Rego (`regal` LSP)
- **AI/LLM Integration**: Integrated with multiple AI tools for coding assistance, summarization, and more.
  - `ellama`
  - `gptel` (configured with Gemini)
  - `acp`, `agent-client`
- **Tooling**:
  - Git integration via `magit` with `forge`.
  - LSP for intelligent code completion and navigation.
  - Debugging support via DAP for Go.
  - Docker and Earthfile support.
  - `direnv` and `.env` file integration.
  - `difftastic` for better diffs.
- **Security**: 1Password integration for secrets management.

## Prerequisites

Before installing, ensure you have the following installed:
- [Emacs](https://www.gnu.org/software/emacs/) (version 27.1 or newer)
- [Doom Emacs](https://github.com/doomemacs/doomemacs#installation) and its dependencies.
- A Nerd Font (e.g., [Fira Code Nerd Font](https://www.nerdfonts.com/font-downloads)) for icons to render correctly.
- External tools used by various packages:
  - `op` (1Password CLI)
  - `ollama` for local LLMs
  - Language-specific LSPs (e.g., `gopls`, `pyright`, `regal`, `markdown-oxide`, `terramate-lsp`)
  - `difftastic`


## Development Notes

### Risor TS Mode

Usage hint. Using additional context for agentic engineering:

```mdx
@https://www.masteringemacs.org/article/lets-write-a-treesitter-major-mode @https://github.com/rubiojr/rca/blob/main/context-large.txt @https://github.com/applejag/tree-sitter-risor
Write a emacs major mode for risor using tree-sitter for syntax highlighting.
```

## Installation (New Machine)

### 1. Install Emacs Plus

This setup uses
[emacs-plus](https://github.com/d12frosted/homebrew-emacs-plus)
installed via Homebrew cask. It provides Emacs 30 with native
compilation, tree-sitter, and full daemon + GUI support.

```sh
brew tap d12frosted/emacs-plus
brew install --cask emacs-plus-app
```

The cask installs **Emacs.app** and **Emacs Client.app** to
`/Applications/`. PATH is injected at install time — Nix,
Homebrew, and direnv paths are available without extra config.

### 2. Install Doom Emacs

```sh
git clone https://github.com/doomemacs/doomemacs ~/.emacs.d
```

### 3. Clone This Config

```sh
git clone <this-repo> ~/.doom.d
```

### 4. Patch Emacs Client.app

The bundled Emacs Client.app references
`/opt/homebrew/bin/emacsclient`, which may not exist if Emacs
was installed only via the cask. Patch it to use the binary
inside Emacs.app:

```sh
cd ~/.doom.d
make patch-emacs-client
```

This compiles the AppleScript at `bin/emacs-client.applescript`
into the app bundle. The patched version:

- Uses `/Applications/Emacs.app/Contents/MacOS/bin/emacsclient`
- Opens a **new frame** each time (not just focusing an existing
  one)
- Handles Finder drag-and-drop and `org-protocol://` URLs

### 5. Sync Doom

Build all packages for the installed Emacs version:

```sh
make sync
```

### 6. Install and Start the Daemon

Register the launchd service so the daemon starts on login:

```sh
make install-daemon
make start-daemon
```

Verify it is running:

```sh
make status-daemon
```

### 7. Open Emacs

Open **Emacs Client.app** from Spotlight, the Dock, or Finder.
Each launch opens a new GUI frame connected to the daemon.

## Emacs Daemon

Emacs runs as a background daemon managed by macOS `launchd`.
All frames connect to the same long-running process, so buffers,
LSP sessions, and agent-shell sessions are always available — no
matter how many windows you open.

### Components

- **`bin/emacs-daemon`** — Starts, stops, restarts, or checks
  the status of the daemon.
- **`bin/restart-daemon`** — Restarts the daemon via launchd.
  Use after `doom sync` or config changes.
- **`bin/emacs-client.applescript`** — Source for the patched
  Emacs Client.app AppleScript.
- **`~/Library/LaunchAgents/org.gnu.emacs.daemon.plist`** —
  launchd service that starts the daemon on login and restarts
  on crash.

### Makefile Targets

```
make sync             # doom sync
make upgrade          # doom upgrade
make install-daemon   # Register the launchd service
make uninstall-daemon # Remove the launchd service
make start-daemon     # Start the daemon now
make stop-daemon      # Stop the daemon
make status-daemon    # Check if the daemon is running
make restart-daemon   # Full restart via launchd
make sync-restart     # doom sync, then restart daemon
make upgrade-restart  # doom upgrade, then restart daemon
make patch-emacs-client # Patch Emacs Client.app AppleScript
```

### Logs

Daemon stdout and stderr are written to:

- `/tmp/emacs-daemon.stdout.log`
- `/tmp/emacs-daemon.stderr.log`

Check these if the daemon fails to start.

## Agent Workspace

All agent-shell sessions are grouped in a single Doom workspace
called **Agents**. This keeps them separated from your code
buffers and lets you switch back and forth. Any ACP-supported
agent (Auggie, OpenCode, Gemini, etc.) runs inside this
workspace.

| Keybinding  | Action                                     |
|-------------|--------------------------------------------|
| `SPC m a w` | Switch to (or create) the Agents workspace |
| `SPC TAB l` | Restore a saved workspace after restart    |

Doom's `persp-mode` persists workspaces across daemon restarts,
so your agent session layout is restored when you reconnect with
Emacs Client.app.

## Shell Setup (zshrc)

vterm shells spawned by Emacs need extra configuration in
`~/.zshrc` to work correctly with directory tracking and shell
hook tools. The ordering matters — direnv must come first, the
vterm integration block second, and tools that rely on
`add-zsh-hook` (Starship, Atuin, etc.) last.

```zsh
# 1. Completion
autoload -Uz compinit
compinit

# 2. Direnv — must be early; it prepends to precmd_functions
#    directly (does not use add-zsh-hook)
eval "$(direnv hook zsh)"

# 3. Emacs vterm integration — only active inside vterm
if [[ "$INSIDE_EMACS" = 'vterm' ]] \
    && [[ -n ${EMACS_VTERM_PATH} ]] \
    && [[ -f ${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh ]]; then
    source "${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh"

    # Restore add-zsh-hook after vterm clobbers it
    # (see Known Issues below)
    unfunction add-zsh-hook 2>/dev/null
    autoload -Uz add-zsh-hook

    # Directory tracking via vterm_cmd (51;E OSC)
    vterm_set_directory() {
        vterm_cmd update-pwd "$(pwd)/"
    }
    precmd_functions+=( vterm_set_directory )
    vterm_set_directory
fi

# 4. Other shell init (fzf, aliases, …)
source <(fzf --zsh)

# 5. Prompt and history — must come after the vterm block so
#    their add-zsh-hook calls use the real function
eval "$(starship init zsh)"
eval "$(zoxide init --cmd cd zsh)"
eval "$(atuin init zsh)"
```

The vterm block is guarded by `$INSIDE_EMACS`, so it is skipped
in regular terminal sessions. Doom's vterm module sets
`EMACS_VTERM_PATH` automatically.

## Known Issues

### vterm breaks `add-zsh-hook` (affects Atuin, Starship, etc.)

The vterm shell integration script (`emacs-vterm-zsh.sh`) contains
this line:

```zsh
add-zsh-hook -Uz chpwd (){ print -Pn "\e]2;%m:%2~\a" }
```

Zsh misparsed the trailing anonymous function `(){ ... }` as a
function redefinition, silently replacing the real `add-zsh-hook`
with a one-liner that only prints a terminal title escape sequence.
Every subsequent call to `add-zsh-hook` — by Starship, Atuin,
Zoxide, or any other tool — becomes a no-op. The symptom is that
shell hooks (preexec, precmd) are never registered, so Atuin
history is never recorded, Starship prompt timing breaks, etc.

The workaround in `~/.zshrc` destroys the broken function and
reloads the real one immediately after sourcing the vterm script:

```zsh
source "${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh"

unfunction add-zsh-hook 2>/dev/null
autoload -Uz add-zsh-hook
```

This only runs inside vterm (`$INSIDE_EMACS = 'vterm'`), so
regular terminal sessions are unaffected.

**Upstream bug**: to be filed against
[emacs-libvterm](https://github.com/akermu/emacs-libvterm).

## References

- [Doomemacs](https://github.com/doomemacs/doomemacs)
- [emacs-plus](https://github.com/d12frosted/homebrew-emacs-plus)
- [Learning via YouTube](https://www.youtube.com/playlist?list=PLhXZp00uXBk4np17N39WvB80zgxlZfVwj)
