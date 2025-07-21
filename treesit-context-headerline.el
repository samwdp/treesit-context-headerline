;;; treesit-context-headerline.el --- Show code context in header line using Tree-sitter -*- lexical-binding: t; -*-

;; Author: Sam Precious <samwdp@gmail.com>
;; URL: https://github.com/samwdp/treesit-context-headerline
;; Keywords: convenience, languages, tools
;; Version: 0.2
;; Package-Requires: ((emacs "29.0") (nerd-icons "0.1.0") (all-the-icons "2.2.0"))

;; This file is not part of GNU Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Show code context in the header line using Emacs Tree-sitter.
;; Displays function/class/conditional context in the header line, left-to-right, outermost to innermost.
;; Works with any language supported by Emacs Tree-sitter.

;;; Code:

(require 'cl-lib)
(require 'treesit)

(defgroup treesit-context-headerline nil
  "Show Tree-sitter code context in the Emacs header line."
  :group 'convenience)

(defcustom treesit-context-headerline-structural-node-types
  '("namespace" "namespace_declaration" "class" "class_declaration" "struct"
    "struct_declaration" "interface" "interface_declaration" "enum"
    "enum_declaration" "function" "function_definition" "method" "procedure"
    "procedure_declaration"
    "method_declaration" "method_definition" "property" "property_declaration"
    "event" "event_declaration" "indexer" "indexer_declaration" "delegate"
    "delegate_declaration" "module" "module_declaration" "subroutine"
    "subroutine_declaration" "subroutine_definition")
  "Structural node types to show in the headerline.  This list is language-agnostic."
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
  "Conditional/control node types to show in the headerline.  This list is language-agnostic."
  :type '(repeat string)
  :group 'treesit-context-headerline)

(defcustom treesit-context-headerline-icons-enabled t
  "Whether to show icons alongside context in the headerline."
  :type 'boolean
  :group 'treesit-context-headerline)

(defcustom treesit-context-headerline-icon-backend 'nerd-icons
  "Which icon backend to use.  Either 'nerd-icons or 'all-the-icons."
  :type '(choice (const :tag "Nerd Icons" nerd-icons)
                 (const :tag "All The Icons" all-the-icons))
  :group 'treesit-context-headerline)

(defcustom treesit-context-headerline-separator ">"
  "Separator between context nodes in the headerline.
Can be a string (e.g. \">\"), or a cons cell of (ICON-NAME . ICON-TYPE), e.g.
  (\"nf-cod-chevron_right\" . nerd-icons)
or
  (\"chevron-right\" . all-the-icons)
If a cons cell, the corresponding icon backend is used to render the icon."
  :type '(choice string
                 (cons (string :tag "Icon Name")
                       (choice (const nerd-icons) (const all-the-icons))))
  :group 'treesit-context-headerline)

(defun treesit-context-headerline--render-separator ()
  "Return the separator string or icon for the headerline, padded with spaces."
  (let ((sep treesit-context-headerline-separator))
    (concat
     " "
     (cond
      ((stringp sep) sep)
      ((and (consp sep)
            (stringp (car sep))
            (symbolp (cdr sep)))
       (pcase (cdr sep)
         ('nerd-icons
          (when (require 'nerd-icons nil t)
            (if (fboundp 'nerd-icons-codicon)
                (nerd-icons-codicon (car sep))
              (nerd-icons-icon-for-name (car sep)))))
         ('all-the-icons
          (when (require 'all-the-icons nil t)
            (if (fboundp 'all-the-icons-octicon)
                (all-the-icons-octicon (car sep))
              (all-the-icons-icon-for-name (car sep)))))
         (_ (car sep))))
      (t (format "%s" sep)))
     " ")))

(defvar treesit-context-headerline--backend-warning-shown nil
  "Whether a warning about missing backend has been shown in this session.")

(defun treesit-context-headerline--backend-available-p (backend)
  "Return non-nil if icon BACKEND is available."
  (pcase backend
    ('nerd-icons (require 'nerd-icons nil t))
    ('all-the-icons (require 'all-the-icons nil t))
    (_ nil)))

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

(defun treesit-context-headerline--get-icon (type backend)
  "Return the icon string for node TYPE from BACKEND, or nil if unavailable."
  (pcase backend
    ('nerd-icons
     (when (require 'nerd-icons nil t)
       (pcase type
         ((or "procedure_declaration" "procedure" "method" "function_definition" "function_declaration" "method_definition" "method_declaration") (nerd-icons-mdicon "nf-md-function"))
         ((or "class" "class_definition" "class_declaration") (nerd-icons-codicon "nf-cod-symbol_class"))
         ((or "struct" "struct_definition") (nerd-icons-codicon "nf-cod-symbol_structure"))
         ((or "interface_definition" "interface_declaration") (nerd-icons-codicon "nf-cod-symbol_interface"))
         ((or "namespace_definition" "module_definition") (nerd-icons-codicon "nf-cod-symbol_namespace"))
         ((or "property" "property_declaration" "property_definition") (nerd-icons-codicon "nf-cod-symbol_property"))
         ((or "variable" "variable_declaration") (nerd-icons-codicon "nf-cod-symbol_variable"))
         ((or "if_statement" "if") (nerd-icons-codicon "nf-cod-question"))
         ((or "for_statement" "foreach_statement" "while_statement")
          (nerd-icons-codicon "nf-cod-sync"))
         ((or "switch" "switch_section" "switch_statement") (nerd-icons-codicon "nf-cod-list_tree"))
         ((or "try" "try_statement") (nerd-icons-codicon "nf-cod-shield"))
         ((or "catch" "catch_clause") (nerd-icons-codicon "nf-cod-bug"))
         (_ nil))))
    ('all-the-icons
     (when (require 'all-the-icons nil t)
       (pcase type
         ((or "procedure" "procedure_declaration" "method" "function_definition" "function_declaration" "method_definition" "method_declaration") (all-the-icons-octicon "zap"))
         ((or "class" "class_definition" "class_declaration") (all-the-icons-octicon "package"))
         ((or "struct" "struct_definition") (all-the-icons-octicon "database"))
         ((or "interface_definition" "interface_declaration") (all-the-icons-octicon "plug"))
         ((or "namespace_definition" "module_definition") (all-the-icons-octicon "repo"))
         ((or "property" "property_declaration" "property_definition") (all-the-icons-octicon "settings"))
         ((or "variable" "variable_declaration") (all-the-icons-octicon "tag"))
         ((or "if_statement" "if") (all-the-icons-octicon "question"))
         ((or "foreach_statement" "for_statement" "while_statement" )
          (all-the-icons-octicon "sync"))
         ((or "switch" "switch_section" "switch_statement") (all-the-icons-octicon "ordered"))
         ((or "try" "try_statement") (all-the-icons-octicon "alert"))
         ((or "catch" "catch_clause") (all-the-icons-octicon "bug"))
         (_ nil))))
    (_ nil)))

(defun treesit-context-headerline--node-label (node)
  "Return label for NODE in the headerline.
For HTML, show the actual tag name for nodes with type 'tag_name'."
  (let* ((type (treesit-node-type node))
         label
         icon)
    (cond
     ;; NEW: For HTML tag_name nodes, show their actual text (the tag name)
     ((equal type "tag_name")
      (setq label (string-trim (treesit-node-text node t))))
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
        (concat (if (eq treesit-context-headerline-icon-backend 'all-the-icons)
                    (concat icon " " label)
                  (concat label " " icon)))
      label)))

(defun treesit-context-headerline--get-nodes-at-point ()
  "Return a list of relevant ancestor nodes at point for context."
  ;; Dummy implementation for illustration; replace with your logic for traversing tree-sitter nodes.
  (let ((root (treesit-buffer-root-node)))
    (when root
      (let ((node (treesit-node-at (point))))
        (cl-loop for n = node then (treesit-node-parent n)
                 while n
                 collect n)))))

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
         (labels (delq nil labels))
         (separator (treesit-context-headerline--render-separator)))
    (if (and labels (> (length labels) 0))
        ;; Reverse labels so left is parent, right is child
        (mapconcat #'identity (reverse labels) separator)
      "")))

(defun treesit-context-headerline--update ()
  "Update the header-line with the context path."
  (treesit-context-headerline--maybe-warn-missing-backend)
  (let ((ctx (treesit-context-headerline--format)))
    (setq header-line-format
          (if (and ctx (not (string-empty-p ctx)))
              ctx
            nil))))

;;;###autoload
(define-minor-mode treesit-context-headerline-mode
  "Minor mode to show Tree-sitter code context in the header line."
  :global nil
  :group 'treesit-context-headerline
  (if treesit-context-headerline-mode
      (progn
        (add-hook 'post-command-hook #'treesit-context-headerline--update nil t)
        (treesit-context-headerline--update))
    (remove-hook 'post-command-hook #'treesit-context-headerline--update t)
    (setq header-line-format nil)))

(provide 'treesit-context-headerline)

;;; treesit-context-headerline.el ends here
