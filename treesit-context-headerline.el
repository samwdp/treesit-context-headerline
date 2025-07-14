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

(defcustom treesit-context-headerline-structural-node-types
  '("namespace" "namespace_declaration" "class" "class_declaration" "struct"
    "struct_declaration" "interface" "interface_declaration" "enum"
    "enum_declaration" "function" "function_definition" "method"
    "method_declaration" "method_definition" "property" "property_declaration"
    "event" "event_declaration" "indexer" "indexer_declaration" "delegate"
    "delegate_declaration" "module" "module_declaration" "program" "subroutine"
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

(defun treesit-context-headerline--node-label (node)
  "Return a label for NODE for the headerline."
  (let* ((type (treesit-node-type node))
         (label nil))
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
    label))

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
        (mapconcat #'identity labels treesit-context-headerline-separator)
      "")))

(defun treesit-context-headerline--update ()
  "Update the header-line with the context path."
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
