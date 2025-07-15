;;; treesit-context-headerline.el --- Show syntax context in headerline using Tree-sitter -*- lexical-binding: t; -*-

;; Author: samwdp
;; Maintainer: samwdp
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; Homepage: https://github.com/samwdp/treesit-context-headerline
;; Keywords: convenience, tools, languages
;; License: MIT

;;; Commentary:

;; treesit-context-headerline-mode displays a context path in the header line,
;; showing the current function, class, and control blocks (such as if, for, foreach, while, etc)
;; using the Tree-sitter parser. It works with any language supported by Tree-sitter.
;; The context is displayed left-to-right, outermost to innermost.

;;; Code:

(require 'treesit)
(require 'cl-lib)

(defgroup treesit-context-headerline nil
  "Show code context path in the header line using Tree-sitter."
  :group 'convenience
  :prefix "treesit-context-headerline-")

(defcustom treesit-context-headerline-separator " > "
  "Separator string for context path in the header line."
  :type 'string
  :group 'treesit-context-headerline)

(defcustom treesit-context-headerline-icons-enabled t
  "Whether to display icons in the context headerline.
When enabled, icons will be shown before each node according to its type,
using the selected icon backend. If the backend is not available,
no icons will be displayed."
  :type 'boolean
  :group 'treesit-context-headerline)

(defcustom treesit-context-headerline-icon-backend 'nerd-icons
  "Icon backend to use for displaying icons in the headerline.
Available options:
- `nerd-icons': Use nerd-icons.el package (default)
- `all-the-icons': Use all-the-icons.el package

The selected backend must be installed for icons to work.
If the selected backend is not available, no icons will be displayed."
  :type '(choice (const :tag "Nerd Icons" nerd-icons)
                 (const :tag "All The Icons" all-the-icons))
  :group 'treesit-context-headerline)

(defcustom treesit-context-headerline-structural-node-types
  '("namespace" "namespace_declaration" "class" "class_declaration" "struct"
    "struct_declaration" "interface" "interface_declaration" "enum"
    "enum_declaration" "function" "function_definition" "method"
    "method_declaration" "method_definition" "property" "property_declaration"
    "event" "event_declaration" "indexer" "indexer_declaration" "delegate"
    "delegate_declaration" "module" "module_declaration" "subroutine"
    "subroutine_declaration" "subroutine_definition")
  "Structural node types to show in the headerline. This list is language-agnostic."
  :type '(repeat string)
  :group 'treesit-context-headerline)

(defcustom treesit-context-headerline-conditional-node-types
  '("if" "if_statement" "else" "else_clause" "switch" "switch_statement" "switch_section"
    "case" "case_switch_label" "default" "default_switch_label" "for" "for_statement"
    "foreach" "foreach_statement" "while" "while_statement" "do" "do_statement"
    "try" "try_statement" "catch" "catch_clause" "finally" "finally_clause"
    "using" "using_statement" "lock" "lock_statement" "checked" "checked_statement"
    "unchecked" "unchecked_statement" "repeat" "repeat_statement" "until" "until_statement"
    "with" "with_statement" "guard" "guard_clause" "match" "match_statement")
  "Conditional/control node types to show in the headerline. This list is language-agnostic."
  :type '(repeat string)
  :group 'treesit-context-headerline)

(defun treesit-context-headerline--get-nodes-at-point ()
  "Return a list of all ancestor nodes at point, from top to innermost."
  (let ((node (ignore-errors (treesit-node-at (point))))
        results)
    (while node
      (push node results)
      (setq node (treesit-node-parent node)))
    (nreverse results))) ;; top-most node first

(defun treesit-context-headerline--backend-available-p (backend)
  "Check if BACKEND is available.
BACKEND can be \\='nerd-icons or \\='all-the-icons."
  (cond
   ((eq backend 'nerd-icons)
    (and (featurep 'nerd-icons)
         (fboundp 'nerd-icons-codicon)))
   ((eq backend 'all-the-icons)
    (and (featurep 'all-the-icons)
         (fboundp 'all-the-icons-octicon)))
   (t nil)))

(defun treesit-context-headerline--safe-get-icon-string (backend family name)
  "Safely get icon string from BACKEND using FAMILY and NAME.
Returns icon string on success, nil on failure."
  (condition-case nil
      (cond
       ((and (eq backend 'nerd-icons) (string= family "codicon"))
        (and (fboundp 'nerd-icons-codicon)
             (nerd-icons-codicon name)))
       ((and (eq backend 'all-the-icons) (string= family "octicon"))
        (and (fboundp 'all-the-icons-octicon)
             (all-the-icons-octicon name)))
       (t nil))
    (error nil)))

(defun treesit-context-headerline--get-fallback-icon (backend)
  "Get a safe fallback icon for BACKEND.
Returns a basic icon that should work or nil if backend unavailable."
  (condition-case nil
      (cond
       ((eq backend 'nerd-icons)
        (and (fboundp 'nerd-icons-codicon)
             (nerd-icons-codicon "chevron-right")))
       ((eq backend 'all-the-icons)
        (and (fboundp 'all-the-icons-octicon)
             (all-the-icons-octicon "chevron-right")))
       (t nil))
    (error nil)))

(defun treesit-context-headerline--get-icon (node-type backend)
  "Get icon for NODE-TYPE using BACKEND.
Returns the icon string or fallback icon if no suitable icon is found.
Uses only verified icon names to prevent errors."
  (when (treesit-context-headerline--backend-available-p backend)
    (let ((icon-map (cond
                     ((eq backend 'nerd-icons)
                      ;; Using verified nerd-icons codicon names that exist
                      '(("function" . ("codicon" . "nf-cod-symbol_function"))
                        ("function_definition" . ("codicon" . "nf-cod-symbol_function"))
                        ("function_declaration" . ("codicon" . "nf-cod-symbol_function"))
                        ("method" . ("codicon" . "nf-cod-symbol_function"))
                        ("method_definition" . ("codicon" . "nf-cod-symbol_function"))
                        ("method_declaration" . ("codicon" . "nf-cod-symbol_function"))
                        ("class" . ("codicon" . "nf-cod-symbol_class"))
                        ("class_declaration" . ("codicon" . "nf-cod-symbol_class"))
                        ("struct" . ("codicon" . "nf-cod-symbol_structure"))
                        ("struct_declaration" . ("codicon" . "nf-cod-symbol_structure"))
                        ("interface" . ("codicon" . "nf-cod-symbol_interface"))
                        ("interface_declaration" . ("codicon" . "nf-cod-symbol_interface"))
                        ("namespace" . ("codicon" . "nf-cod-symbol_namespace"))
                        ("namespace_declaration" . ("codicon" . "nf-cod-symbol_namespace"))
                        ("module" . ("codicon" . "nf-cod-file_submodule"))
                        ("module_declaration" . ("codicon" . "nf-cod-file_submodule"))
                        ("enum" . ("codicon" . "nf-cod-symbol_enum"))
                        ("enum_declaration" . ("codicon" . "nf-cod-symbol_enum"))
                        ("property" . ("codicon" . "nf-cod-symbol_property"))
                        ("property_declaration" . ("codicon" . "nf-cod-symbol_property"))
                        ("variable" . ("codicon" . "nf-cod-symbol_variable"))
                        ("variable_declaration" . ("codicon" . "nf-cod-symbol_variable"))
                        ("if" . ("codicon" . "nf-cod-question"))
                        ("if_statement" . ("codicon" . "nf-cod-question"))
                        ("for" . ("codicon" . "nf-cod-sync"))
                        ("for_statement" . ("codicon" . "nf-cod-sync"))
                        ("foreach" . ("codicon" . "nf-cod-sync"))
                        ("foreach_statement" . ("codicon" . "nf-cod-sync"))
                        ("while" . ("codicon" . "nf-cod-sync"))
                        ("while_statement" . ("codicon" . "nf-cod-sync"))
                        ("switch" . ("codicon" . "nf-cod-list_tree"))
                        ("switch_statement" . ("codicon" . "nf-cod-list_tree"))
                        ("try" . ("codicon" . "nf-cod-shield"))
                        ("try_statement" . ("codicon" . "nf-cod-shield"))
                        ("catch" . ("codicon" . "nf-cod-debug_alt"))
                        ("catch_clause" . ("codicon" . "nf-cod-debug_alt"))
                        ("default" . ("codicon" . "nf-cod-circle_filled"))))
                     ((eq backend 'all-the-icons)
                      ;; Using verified all-the-icons octicon names that exist
                      '(("function" . ("octicon" . "zap"))
                        ("function_definition" . ("octicon" . "zap"))
                        ("function_declaration" . ("octicon" . "zap"))
                        ("method" . ("octicon" . "zap"))
                        ("method_definition" . ("octicon" . "zap"))
                        ("method_declaration" . ("octicon" . "zap"))
                        ("class" . ("octicon" . "package"))
                        ("class_declaration" . ("octicon" . "package"))
                        ("struct" . ("octicon" . "database"))
                        ("struct_declaration" . ("octicon" . "database"))
                        ("interface" . ("octicon" . "plug"))
                        ("interface_declaration" . ("octicon" . "plug"))
                        ("namespace" . ("octicon" . "file-directory"))
                        ("namespace_declaration" . ("octicon" . "file-directory"))
                        ("module" . ("octicon" . "file-submodule"))
                        ("module_declaration" . ("octicon" . "file-submodule"))
                        ("enum" . ("octicon" . "list-unordered"))
                        ("enum_declaration" . ("octicon" . "list-unordered"))
                        ("property" . ("octicon" . "gear"))
                        ("property_declaration" . ("octicon" . "gear"))
                        ("variable" . ("octicon" . "primitive-dot"))
                        ("variable_declaration" . ("octicon" . "primitive-dot"))
                        ("if" . ("octicon" . "nf-cod-question"))
                        ("if_statement" . ("octicon" . "question"))
                        ("for" . ("octicon" . "nf-cod-sync"))
                        ("for_statement" . ("octicon" . "nf-cod-sync"))
                        ("foreach" . ("octicon" . "sync"))
                        ("foreach_statement" . ("octicon" . "sync"))
                        ("while" . ("octicon" . "sync"))
                        ("while_statement" . ("octicon" . "sync"))
                        ("switch" . ("octicon" . "list-ordered"))
                        ("switch_statement" . ("octicon" . "list-ordered"))
                        ("try" . ("octicon" . "nf-cod-shield"))
                        ("try_statement" . ("octicon" . "shield"))
                        ("catch" . ("octicon" . "alert"))
                        ("catch_clause" . ("octicon" . "alert"))
                        ("default" . ("octicon" . "dot-fill"))))
                     (t nil))))
      ;; Try to get icon for specific node type
      (let ((icon nil))
        (when-let* ((icon-spec (cdr (assoc node-type icon-map))))
          (let ((family (car icon-spec))
                (name (cdr icon-spec)))
            (setq icon (treesit-context-headerline--safe-get-icon-string backend family name))))
        ;; If no icon found for specific type, try fallback
        (or icon (treesit-context-headerline--get-fallback-icon backend))))))

(defvar treesit-context-headerline--backend-warning-shown nil
  "Whether a warning about missing backend has been shown in this session.")

(defun treesit-context-headerline--maybe-warn-missing-backend ()
  "Show a warning if icons are enabled but the selected backend is not available."
  (when (and treesit-context-headerline-icons-enabled
             (not treesit-context-headerline--backend-warning-shown)
             (not (treesit-context-headerline--backend-available-p 
                   treesit-context-headerline-icon-backend)))
    (setq treesit-context-headerline--backend-warning-shown t)
    (display-warning 'treesit-context-headerline
                     (format "Icon backend '%s' is not available. Install the package or disable icons."
                             treesit-context-headerline-icon-backend)
                     :warning)))

(defun treesit-context-headerline--node-label (node)
  "Return a label for NODE for the headerline."
  (let* ((type (treesit-node-type node))
         (label nil)
         (icon nil))
    (cond
     ;; For conditionals, prettify type
     ((member type treesit-context-headerline-conditional-node-types)
      (setq label (replace-regexp-in-string
                   "_statement\\|_clause\\|_section\\|_label\\|_declaration\\|_definition" "" type)))
     ;; For structural, use name/identifier if possible
     ((member type treesit-context-headerline-structural-node-types)
      (let ((name-node (or (treesit-node-child-by-field-name node "name")
                           (treesit-node-child-by-field-name node "identifier"))))
        (setq label
              (if (and name-node
                       (let ((txt (string-trim (treesit-node-text name-node t))))
                         (and (not (string-empty-p txt)) txt)))
                  (string-trim (treesit-node-text name-node t))
                (replace-regexp-in-string
                 "_statement\\|_clause\\|_section\\|_label\\|_declaration\\|_definition" "" type)))))
     (t nil))
    
    ;; Add icon if enabled and available
    (when (and label treesit-context-headerline-icons-enabled)
      (setq icon (treesit-context-headerline--get-icon type treesit-context-headerline-icon-backend)))
    
    ;; Combine icon and label
    (if icon
        (concat icon " " label)
      label)))

(defun treesit-context-headerline--format ()
  "Return the context path as a string for the headerline, including all matching block ancestors."
  (let* ((nodes (treesit-context-headerline--get-nodes-at-point))
         (labels
          (cl-loop for node in nodes
                   for type = (treesit-node-type node)
                   when (or (member type treesit-context-headerline-structural-node-types)
                            (member type treesit-context-headerline-conditional-node-types))
                   collect (treesit-context-headerline--node-label node)))
         ;; Remove nils (unlabeled nodes)
         (labels (delq nil labels)))
    (if (and labels (> (length labels) 0))
        ;; Reverse labels so left is parent, right is child
        (mapconcat #'identity (reverse labels) treesit-context-headerline-separator)
      "")))

(defun treesit-context-headerline--update ()
  "Update the header-line with the context path."
  (treesit-context-headerline--maybe-warn-missing-backend)
  (let ((ctx (treesit-context-headerline--format)))
    (setq header-line-format
          (when (and ctx (not (string-empty-p ctx)))
            `((:eval ,ctx))))))

;;;###autoload
(define-minor-mode treesit-context-headerline-mode
  "Show current code context path in the header line using Tree-sitter.
Works for any language with a Tree-sitter grammar."
  :lighter " CtxHdr"
  (if treesit-context-headerline-mode
      (progn
        (add-hook 'post-command-hook #'treesit-context-headerline--update nil t)
        (treesit-context-headerline--update))
    (remove-hook 'post-command-hook #'treesit-context-headerline--update t)
    (kill-local-variable 'header-line-format)))

(provide 'treesit-context-headerline)
;;; treesit-context-headerline.el ends here
