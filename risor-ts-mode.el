;;; risor-ts-mode.el --- Major mode for Risor files using tree-sitter -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Cline

;; Author: Cline
;; Keywords: languages, modes, tree-sitter
;; Package-Requires: ((emacs "29.1") (tree-sitter "0.20"))
;; URL: https://github.com/applejag/tree-sitter-risor

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Major mode for editing Risor files with tree-sitter for syntax highlighting
;; and indentation.

;;; Code:

(require 'treesit)
(require 'prog-mode) ; Risor is a programming language, so derive from prog-mode

;;;###autoload
(define-derived-mode risor-ts-mode prog-mode "Risor[ts]"
  "Major mode for editing Risor files with tree-sitter."
  :group 'risor-ts
  :syntax-table nil ; Use tree-sitter for syntax, not traditional syntax table

  (setq-local font-lock-defaults nil) ; Disable traditional font-lock

  (when (treesit-ready-p 'risor)
    (treesit-parser-create 'risor)
    (risor-ts-setup)))

(defun risor-ts-setup ()
  "Setup tree-sitter for `risor-ts-mode`."
  ;; Font locking
  (setq-local treesit-font-lock-settings
              (apply #'treesit-font-lock-rules
                     risor-ts-font-lock-rules))

  (setq-local treesit-font-lock-feature-list
              '((comment)
                (keyword string constant)
                (function variable)
                (operator delimiter)))

  ;; Indentation
  (setq-local treesit-simple-indent-rules risor-ts-indent-rules)

  ;; Imenu (optional)
  (setq-local treesit-simple-imenu-settings risor-ts-imenu-settings)

  ;; Final setup call
  (treesit-major-mode-setup))

;; Define font lock rules (to be populated)
(defvar risor-ts-font-lock-rules
  '(:language risor
    :feature comment
    ((comment) @font-lock-comment-face)

    :language risor
    :feature keyword
    (["func" "return" "if" "else" "for" "switch" "case" "default" "break" "continue" "go" "defer" "import" "var" "const"] @font-lock-keyword-face
     (true) @font-lock-keyword-face
     (false) @font-lock-keyword-face
     (nil) @font-lock-keyword-face)

    :language risor
    :feature operator
    ((binary_expression operator: _) @font-lock-operator-face
     (unary_expression operator: _) @font-lock-operator-face
     (assignment_statement operator: _) @font-lock-operator-face
     (inc_statement "++" @font-lock-operator-face)
     (dec_statement "--" @font-lock-operator-face)
     (short_var_declaration ":=" @font-lock-operator-face)
     (conditional_expression "?" @font-lock-operator-face)
     (conditional_expression ":" @font-lock-operator-face))

    :language risor
    :feature string
    ((string) @font-lock-string-face
     (string_template) @font-lock-string-face
     (string_backtick) @font-lock-string-face
     (escape_sequence) @font-lock-escape-face
     (format_sequence) @font-lock-preprocessor-face
     (string_template_argument) @font-lock-variable-name-face)

    :language risor
    :feature constant
    ((int_literal) @font-lock-constant-face
     (float_literal) @font-lock-constant-face)

    :language risor
    :feature function
    ((function_declaration name: _) @font-lock-function-name-face
     (call_expression function: _) @font-lock-function-call-face)

    :language risor
    :feature variable
    ((parameter_declaration name: _) @font-lock-variable-name-face
     (var_declaration name: _) @font-lock-variable-name-face
     (const_declaration name: _) @font-lock-variable-name-face
     (short_var_declaration left: _) @font-lock-variable-name-face
     (identifier) @font-lock-variable-name-face) ; General identifier, might be refined later

    :language risor
    :feature delimiter
    (["(" ")" "[" "]" "{" "}"] @font-lock-bracket-face)
    ))

;; Define indentation rules (to be populated)
(defvar risor-ts-indent-rules
  '((risor
     ;; Root node
     ((parent-is "source_file") column-0 0)

     ;; Blocks (function bodies, if/for/switch statements)
     ((parent-is "block") parent 2)
     ((node-is "}") parent 0)

     ;; Function parameters
     ((parent-is "parameter_list") parent 2)
     ((node-is ")") parent 0)

     ;; List and map/set literals
     ((parent-is "list_literal") parent 2)
     ((node-is "]") parent 0)
     ((parent-is "map_or_set_literal") parent 2)
     ((node-is "}") parent 0)

     ;; Case statements in switch
     ((node-is "switch_case") parent 0)
     ((node-is "default_case") parent 0)

     ;; Statements within blocks
     ((node-is "_statement") parent 0)
     ((node-is "_simple_statement") parent 0)
     ((node-is "_declaration") parent 0)
     ((node-is "expression_statement") parent 0)
     ((node-is "inc_statement") parent 0)
     ((node-is "dec_statement") parent 0)
     ((node-is "assignment_statement") parent 0)
     ((node-is "return_statement") parent 0)
     ((node-is "go_statement") parent 0)
     ((node-is "defer_statement") parent 0)
     ((node-is "import_statement") parent 0)
     ((node-is "if_statement") parent 0)
     ((node-is "for_statement") parent 0)
     ((node-is "switch_statement") parent 0)
     ((node-is "break_statement") parent 0)
     ((node-is "continue_statement") parent 0)

     ;; For clause
     ((parent-is "for_clause") parent 2)

     ;; Map literal pairs
     ((node-is "map_literal_pair") parent 0)

     ;; Default catch-all
     (no-node parent 0))))

;; Define Imenu settings (to be populated)
(defvar risor-ts-imenu-settings
  '(("Functions" risor-ts-imenu-function-p nil risor-ts-imenu-name-function)))

(defun risor-ts-imenu-function-p (node)
  "Return non-nil if NODE is a function definition for Imenu."
  ;; Placeholder: Implement logic to identify function nodes
  ;; based on Risor grammar.
  (and (treesit-node-type-p node "function_declaration")
       (treesit-node-child node 1))) ; Assuming child 1 is the function name

(defun risor-ts-imenu-name-function (node)
  "Return the name for Imenu entry from NODE."
  ;; Placeholder: Extract function name from the node.
  (when (risor-ts-imenu-function-p node)
    (treesit-node-text (treesit-node-child node 1))))

(provide 'risor-ts-mode)
;;; risor-ts-mode.el ends here
