;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Matthias Scholz"
      user-mail-address "matthias.scholz@thoughtworks.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Have fine undo instead of the coarse vim default undo until last insert mode
;; https://www.reddit.com/r/DoomEmacs/comments/r78bwz/comment/hmxqjzb/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
(setq evil-want-fine-undo t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;; (setq org-directory "~/org/")
;; (setq org-directory "~/Documents/Notes")

;; golang
;; https://emacs-lsp.github.io/dap-mode/page/configuration/#go
;; if you want to debug current test function inside test file use "Go Dlv Test Current Function Configuration"
(require 'dap-dlv-go)

;; customize evil-mode
;; https://docs.doomemacs.org/latest/modules/editor/evil/
(setq evil-move-cursor-back nil)

;; activate multiedit default shortcuts
;; https://github.com/hlissner/evil-multiedit
;; Select:
;; M-d: Select current word and move to next (exact match) - no visual mode required
;; R: select all matches done via visual mode
;; Perform changes:
;; I: insert at the beginning
;; P: replace with clipboard content
;; D: delete
;; FIXME (evil-multiedit-default-keybinds)

;; This is so buffers auto-save
;; auto save
;; https://joeprevite.com/doom-emacs/
;; (setq auto-save-visited-interval 5)
;; (auto-save-visited-mode +1)

;; Save the session frequently
;; https://emacs.stackexchange.com/questions/46963/how-i-can-make-persp-mode-save-my-workspace-on-every-change
;; runs after 10s of idle time
;; (run-with-idle-timer 10 t #'doom-save-session)
;; or every 10s, period
;; (run-at-time 10 t #'doom-save-session)


;; Support to open reference lookup on a different frame or window
;; https://github.com/doomemacs/doomemacs/issues/3397
(dolist (fn '(definition references))
  (fset (intern (format "+lookup/%s-other-window" fn))
        (lambda (identifier &optional arg)
          "TODO"
          (interactive (list (doom-thing-at-point-or-region)
                             current-prefix-arg))
          (let ((pt (point)))
            (switch-to-buffer-other-window (current-buffer))
            (goto-char pt)
            (funcall (intern (format "+lookup/%s" fn)) identifier arg)))))
(define-key evil-normal-state-map "god" '+lookup/definition-other-window)

;; Change default project search key binding
(map! :leader
      :desc "Search project"
      "s p" #'+vertico/project-search)
;; TODO Understand why this is not configured as the default since vertico is set.
(map! :leader
      :desc "Search current directory"
      "s d" #'+vertico/project-search-from-cwd)

;; Earthly syntax highlighting support
;; (use-package! earthfile-mode
;;  :mode "Earthfile")

;; Mermaid support - NOTE use go-grip instead
;; (use-package! mermaid-mode
;;:config
;;  (setq mermaid-mmdc-location "docker")
;;  (setq mermaid-flags "run -u 1000 -v /tmp:/tmp ghcr.io/mermaid-js/mermaid-cli/mermaid-cli:9.1.6"))

;; Ellama configuration
;; https://github.com/s-kostyaev/ellama
;; https://discourse.doomemacs.org/t/how-to-re-bind-keys/56
;; https://willschenk.com/labnotes/2024/ai_in_emacs/
;; https://www.reddit.com/r/emacs/comments/j9vxvd/general_add_description_to_prefix_keys/
(use-package! ellama
  :init
  (setopt ellama-keymap-prefix "C-c a"))
(map! :leader
      "m a" '(:ignore t :which-key "interact with ai")
      :prefix "m a"
      :desc "open chat" "t" #'ellama-chat
      :desc "summarize" "s" #'ellama-summarize
      :desc "ask" "a" #'ellama-ask-about
      :desc "define" "d" #'ellama-define-word
      :desc "commit msg" "g" #'ellama-generate-commit-message
      "c" '(:ignore t :which-key "coding")
      :desc "description to code" "a" #'ellama-code-add
      :desc "review" "r" #'ellama-code-review
      :desc "improve" "i" #'ellama-code-improve
      :desc "complete" "c" #'ellama-code-complete
      "i" '(:ignore t :which-key "improve writing")
      :desc "grammar" "i g" #'ellama-improve-grammar
      :desc "wording" "i w" #'ellama-improve-wording
      :desc "conciseness" "i c" #'ellama-improve-conciseness
      )
;; ellama-complete ???

;; Listing of todos for projects
;; FIXME it seems it creates problems
;; (use-package! magit-todos
;;   :after magit
;;   :config
;;   (magit-todos-mode 1)
;;   ;; FIXME seems to create error: function definition is void:
;;   (setq magit-todos-group-by "By keyword")
;;   (setq magit-todos-keyword-suffix "\\(?:([^)]+)\\)?:?") ; make colon optional
;;   (setq magit-todos-exclude-globs "*.js.map") ; ignore javascript compilations
;;   (define-key magit-todos-section-map "j" nil ))

;; Terramate specific file extension as terraform
(add-to-list 'auto-mode-alist '("\\.tmgen\\'" . terraform-mode))
;; HACK support terraform buffer formatting
;; FIXME support hcl buffer formatting in general
(add-to-list 'auto-mode-alist '("\\.tm.hcl\\'" . terraform-mode))

;; Configure Difftastic
(use-package! difftastic-bindings
  :ensure difftastic ;; or nil if you prefer manual installation
  :config (difftastic-bindings-mode))

;; Open Policy Agent lsp and linting support using regal
;; SEE: https://emacs-lsp.github.io/lsp-mode/page/adding-new-language/
;; SEE: https://docs.styra.com/regal/language-server#features
(add-to-list 'lsp-language-id-configuration '(rego-mode . "open-policy-agent"))

(lsp-register-client (make-lsp-client
                      :new-connection (lsp-stdio-connection '("regal" "language-server"))
                      :activation-fn (lsp-activate-on "open-policy-agent")
                      :server-id 'regal))


;; Terramate Language Server
(add-to-list 'lsp-language-id-configuration '(terraform-mode . "terramate"))
(add-to-list 'lsp-language-id-configuration '(hcl-mode . "terramate"))

(lsp-register-client (make-lsp-client
                      :new-connection (lsp-stdio-connection '("terramate-lsp"))
                      :activation-fn (lsp-activate-on "terramate")
                      :server-id 'terrmate))


;; Interactive JSON with jq
;; https://github.com/ljos/jq-mode
(use-package! jq-mode)
(setq jq-interactive-command "yq"
      jq-interactive-font-lock-mode #'yaml-mode
      jq-interactive-default-options "--yaml-roundtrip")

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
