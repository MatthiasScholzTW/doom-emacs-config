;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
                                        ;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
                                        ;(package! another-package
                                        ;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
                                        ;(package! this-package
                                        ;  :recipe (:host github :repo "username/repo"
                                        ;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
                                        ;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
                                        ;(package! builtin-package :recipe (:nonrecursive t))
                                        ;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
                                        ;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
                                        ;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
                                        ;(unpin! pinned-package)
;; ...or multiple packages
                                        ;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
                                        ;(unpin! t)

;; https://github.com/syl20bnr/evil-tutor
;; (package! evil-tutor)

(package! mermaid-mode)
(package! earthfile-mode)

;; TODO Get dotenv working
;;(package! project-env
;;  :recipe (:local-repo "lisp/dotenv"))
(package! dotenv
  :recipe (:host github :repo "pkulev/dotenv.el"))

;; https://github.com/janusvm/emacs-gitmoji
;; M-x gitmoji-insert-emoji
;; TODO bound to SPC i g (for "insert Gitmoji").
(package! gitmoji
  :recipe (:host github
           :repo "janusvm/emacs-gitmoji"
           :files ("*.el" "data")))

;; Open Policy Agent
(package! rego-mode)

;; Focus Mode
;; https://github.com/larstvei/Focus
(package! focus)

;; Local AI model integration vio ollama
;; Requires ollama to be running
;; using `ollama pull zephyr`
(package! ellama)
;;  :ensure t
;;  :init
;;  ;; setup key bindings
;;  (setopt ellama-keymap-prefix "C-c e"))

;; Listing of todos per project
(package! magit-todos)

;; Difftastic
(package! difftastic)

;; Interactive JSON filtering using jq
;; https://github.com/ljos/jq-mode
(package! jq-mode)

;; LLM client
;; https://github.com/karthink/gptel?tab=readme-ov-file#doom-emacs
(package! gptel :recipe (:nonrecursive t))
(package! mcp)

;; AI Agent
;; https://github.com/MatthewZMD/aidermacs
(package! aidermacs)
;; alternative:
;; https://github.com/tninja/aider.el/tree/main
(package! aider :recipe (:host github :repo "tninja/aider.el" ))

;; ACP Agent
;; https://github.com/xenodium/agent-shell
(package! shell-maker)
(package! acp :recipe (:host github :repo "xenodium/acp.el" :files ("*.el")))
(package! agent-shell :recipe (:host github :repo "xenodium/agent-shell" :files ("*.el")))
(package! agent-shell-manager :recipe (:host github :repo "jethrokuan/agent-shell-manager" :files ("*.el")))
(package! agent-shell-sidebar :recipe (:host github :repo "cmacrae/agent-shell-sidebar" :files ("*.el")))
(package! agent-review :recipe (:host github :repo "nineluj/agent-review" :files ("*.el")))
(package! agent-shell-attention :recipe (:host github :repo "ultronozm/agent-shell-attention.el" :files ("*.el")))

;; 1-Password Connector
(package! auth-source-1password
  :recipe (:host github
           :repo "dlobraico/auth-source-1password"
           :files ("*.el")))

;; Obsidian Vault Support
;; https://github.com/licht1stein/obsidian.el
(package! obsidian)

;; REST API Client: hurl
;; https://hurl.dev
;; https://github.com/JasZhe/hurl-mode
(package! hurl-mode :recipe (:host github :repo "jaszhe/hurl-mode" :files ("*.el")))

;; Kanata Configuration File Support
(package! kanata-kbd-mode :recipe (:host github :repo "chmouel/kanata-kbd-mode" :files ("*.el")))

;; TODO not working since the grammar can not be compiled
;; Risor TS mode
(package! risor-ts-mode
  :recipe (:local-repo "."
           :files ("risor-ts-mode.el")))

;; NOTE Deprecated moved to obsidian plugin
;; RSS Feed Reader Optimization
;; (package! elfeed-goodies)

;; Kubernetes Cluster Management
;; https://github.com/jinnovation/kele.el
(package! kele)
