;;; tcl-ts-mode.el --- Tree-sitter support for Tcl  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Andrew Peck

;; Author: Andrew Peck <me@andrewpeck.xyz>
;; Keywords: languages tcl tree-sitter
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; `tcl-ts-mode' is a thin tree-sitter layer on top of the stock
;; `tcl-mode' from tcl.el.  It derives from `tcl-mode', so the keymap,
;; menu, indentation engine (`tcl-indent-line'), electric characters,
;; and the `inferior-tcl' commands all behave exactly as before.
;; Fontification, Imenu, `add-log-current-defun' and the quote/comment
;; syntax are replaced by tree-sitter equivalents.
;;
;; Driving the syntax table from the parse tree is what fixes bracket
;; matching for a command substitution nested inside a string, which
;; Vivado and similar generators emit constantly:
;;
;;   set files [list \
;;    "[file normalize "$origin_dir/IP/clk_wiz_0.xci"]"\
;;    ]
;;
;; tcl.el mis-lexes that, its bracket depth goes negative, and every
;; following line in the file loses its indentation.  See
;; `tcl-ts-mode--quote-syntax'.
;;
;; Defun navigation is left to tcl.el on purpose.  `tcl-calculate-indent'
;; and `tcl-in-comment' use `beginning-of-defun' to pick the position
;; they start parsing from, so replacing it with
;; `treesit-beginning-of-defun' -- which can land inside a brace group --
;; changes how lines indent.  Keeping tcl.el's version means `C-M-a',
;; `mark-defun', `tcl-eval-defun' and indentation all agree with
;; `tcl-mode'.  If you would rather have parse-tree navigation and can
;; live with the indentation differences, add to `tcl-ts-mode-hook':
;;
;;   (setq-local beginning-of-defun-function #'treesit-beginning-of-defun
;;               end-of-defun-function #'treesit-end-of-defun)
;;
;; Faces follow tcl.el's choices, so a buffer in `tcl-ts-mode' looks
;; like a `tcl-mode' buffer -- but the parse tree means that `#' inside
;; a word, quotes inside braced words, and `$var(index)' references are
;; classified correctly instead of by regexp.
;;
;; Indentation is `tcl-indent-line' throughout and agrees with `tcl-mode'
;; on any file tcl.el lexes correctly.  It differs only where the fixed
;; quote syntax gives the indenter a correct bracket depth to work from.
;;
;; Because the keyword lists are read when the mode starts, customizing
;; `tcl-keyword-list', `tcl-builtin-list', `tcl-typeword-list' or
;; `tcl-proc-list' takes effect the next time `tcl-ts-mode' is enabled.
;;
;; Setup:
;;
;;   (require 'tcl-ts-mode)
;;   (treesit-install-language-grammar 'tcl)   ; once
;;
;; Loading this file remaps `tcl-mode' to `tcl-ts-mode' via
;; `major-mode-remap-alist' when the grammar is available; it does not
;; touch `auto-mode-alist', so tcl.el keeps owning the file
;; associations.
;;
;; Known limitations:
;;
;; * A backslash at end of line is an "extra" in the grammar rather than
;;   a node, so it cannot be queried and does not get the
;;   `tcl-escaped-newline' face that `tcl-mode' gives it.
;;
;; * The grammar does not accept every braced word.  Regexp and glob
;;   patterns containing `(' or `[' (`regexp {^v(\d+)\.(\d+)$} ...'),
;;   `set' with a braced variable name (`set {::syntax(a b)} c'), and
;;   `try ... on ok' all produce ERROR nodes.  Such regions simply get
;;   less highlighting; measured over ~600k characters of Tcl they
;;   accounted for about 2.5% of the text.  Indentation is unaffected,
;;   since it comes from tcl.el and not from the parse tree.

;;; Code:

(require 'tcl)
(require 'treesit)

(declare-function treesit-node-child "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-parser-create "treesit.c")

(add-to-list 'treesit-language-source-alist
             '(tcl "https://github.com/tree-sitter-grammars/tree-sitter-tcl"
                   "main" "src")
             t)


;;; Font lock.

(defun tcl-ts-mode--anchored-opt (words)
  "Return a regexp matching exactly any string in WORDS."
  (concat "\\`" (regexp-opt words) "\'"))

(defun tcl-ts-mode--font-lock-settings ()
  "Return `treesit-font-lock-settings' for `tcl-ts-mode'.
Keywords are taken from `tcl-keyword-list', `tcl-builtin-list' and
`tcl-typeword-list', so that customizing those lists also affects
tree-sitter fontification."
  (treesit-font-lock-rules
   :language 'tcl
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'tcl
   :feature 'definition
   '((procedure name: (_) @font-lock-function-name-face)
     (argument name: (_) @font-lock-variable-name-face))

   :language 'tcl
   :feature 'keyword
   `(["proc" "if" "else" "elseif" "while" "foreach"
      "try" "on" "finally" "catch" "error"]
     @font-lock-keyword-face
     ((command name: (simple_word) @font-lock-keyword-face)
      (:match ,(tcl-ts-mode--anchored-opt tcl-keyword-list)
              @font-lock-keyword-face)))

   :language 'tcl
   :feature 'string
   '((quoted_word) @font-lock-string-face)

   :language 'tcl
   :feature 'type
   `(["global"] @font-lock-type-face
     ((command name: (simple_word) @font-lock-type-face)
      (:match ,(tcl-ts-mode--anchored-opt tcl-typeword-list)
              @font-lock-type-face)))

   :language 'tcl
   :feature 'builtin
   `(["set" "expr" "regexp" "namespace"] @font-lock-builtin-face
     ((command name: (simple_word) @font-lock-builtin-face)
      (:match ,(tcl-ts-mode--anchored-opt tcl-builtin-list)
              @font-lock-builtin-face))
     ((simple_word) @font-lock-builtin-face
      (:match "\\`\\(?:argc\\|argv0?\\|env\\|errorCode\\|errorInfo\\|\
auto_path\\|tcl_[a-zA-Z]+\\)\'"
              @font-lock-builtin-face)))

   :language 'tcl
   :feature 'constant
   ;; Deliberately not "yes"/"no"/"on"/"off": Tcl accepts them as
   ;; booleans, but they are far more often ordinary argument words.
   '(((simple_word) @font-lock-constant-face
      (:match "\\`\\(?:true\\|false\\)\'" @font-lock-constant-face)))

   :language 'tcl
   :feature 'number
   '((number) @font-lock-number-face)

   :language 'tcl
   :feature 'variable
   '((variable_substitution) @font-lock-variable-use-face
     (set (id) @font-lock-variable-name-face))

   :language 'tcl
   :feature 'function
   '((command name: (simple_word) @font-lock-function-call-face))

   :language 'tcl
   :feature 'escape-sequence
   :override t
   '((escaped_character) @font-lock-escape-face)

   :language 'tcl
   :feature 'operator
   '(["**" "/" "*" "%" "+" "-" "<<" ">>" ">" "<" ">=" "<="
      "==" "!=" "eq" "ne" "in" "ni" "&" "^" "|" "&&" "||"
      "~" "!" "?" ":"]
     @font-lock-operator-face)

   :language 'tcl
   :feature 'bracket
   '(["{" "}" "[" "]" "(" ")"] @font-lock-bracket-face)

   :language 'tcl
   :feature 'delimiter
   '([";"] @font-lock-delimiter-face)

   :language 'tcl
   :feature 'misc-punctuation
   '((unpack) @font-lock-misc-punctuation-face)))


;;; Syntax.

;; tcl.el decides what a `"' or a `#' means with a regexp, which cannot
;; see that a command substitution nested inside a string starts a fresh
;; word.  The parse tree can, so we drive the syntax table from it.

(defconst tcl-ts-mode--syntax-punctuation (string-to-syntax ".")
  "Syntax to give a quote or hash that is an ordinary character.")

(defun tcl-ts-mode--covering-node (pos)
  "Return the leaf node covering POS, or nil if none does.
`treesit-node-at' answers with the following leaf when POS itself is not
covered, which happens around ERROR nodes; reject that case."
  (let ((node (treesit-node-at pos 'tcl)))
    (and node
         (<= (treesit-node-start node) pos)
         (< pos (treesit-node-end node))
         node)))

(defun tcl-ts-mode--under-error-p (node)
  "Return non-nil if NODE is, or is inside, an ERROR node."
  (treesit-parent-until
   node (lambda (n) (equal (treesit-node-type n) "ERROR")) t))

(defun tcl-ts-mode--outermost-quoted-word (node)
  "Return the outermost `quoted_word' at or above NODE, or nil."
  (let (found)
    (while node
      (when (equal (treesit-node-type node) "quoted_word")
        (setq found node))
      (setq node (treesit-node-parent node)))
    found))

(defun tcl-ts-mode--quote-syntax (pos)
  "Return the `syntax-table' value for the double quote at POS.
A nil result keeps the string-delimiter meaning from the syntax table.

A Tcl quoted word is a single token, so only the two outer quotes of the
outermost `quoted_word' delimit a string; every quote nested inside it is
an interior character.  That is what makes a command substitution inside
a string work:

    set f \"[file normalize \"$dir/x.xci\"]\"

The whole word becomes one string, so its brackets are interior and the
bracket depth stays balanced.  tcl.el instead demotes the two inner
quotes, because neither follows one of its word delimiters, which leaves
the `]' counting as live code and drives the depth negative.  See the
FIXME above `tcl-syntax-propertize-function'.

Asking the enclosing word rather than the quote itself also absorbs the
quotes the grammar invents inside a braced regexp such as
{\\+incdir\\+\"[^\"]+\"}: they become interior to one string instead of
unbalancing the line, even where the grammar left an ERROR behind.

With no enclosing word and nothing else to go on, defer to
`tcl--syntax-of-quote', so no buffer ends up worse than in `tcl-mode'."
  (let* ((node (tcl-ts-mode--covering-node pos))
         (word (and node (tcl-ts-mode--outermost-quoted-word node))))
    (cond
     (word
      (unless (or (eq pos (treesit-node-start word))
                  (eq pos (1- (treesit-node-end word))))
        tcl-ts-mode--syntax-punctuation))
     ;; Part of a comment or an escaped character, so it delimits nothing.
     ((and node (not (equal (treesit-node-type node) "\"")))
      tcl-ts-mode--syntax-punctuation)
     (t (tcl--syntax-of-quote pos)))))

(defun tcl-ts-mode--bare-hash-p (pos)
  "Return non-nil if the hash at POS cannot start a comment.
This is tcl.el's rule, used where the tree cannot be trusted: a `#' only
opens a comment at the start of a command."
  (save-excursion
    (goto-char pos)
    (skip-chars-backward " \t")
    (not (memq (char-before) '(nil ?\[ ?\; ?{ ?\n)))))

(defun tcl-ts-mode--hash-syntax (pos)
  "Return the `syntax-table' value for the hash at POS.
A nil result keeps the comment-starter meaning from the syntax table."
  (let ((node (tcl-ts-mode--covering-node pos)))
    (cond
     ((or (null node) (tcl-ts-mode--under-error-p node))
      (and (tcl-ts-mode--bare-hash-p pos) tcl-ts-mode--syntax-punctuation))
     ((and (equal (treesit-node-type node) "comment")
           (eq pos (treesit-node-start node)))
      nil)
     (t tcl-ts-mode--syntax-punctuation))))

(defun tcl-ts-mode--syntax-propertize (start end)
  "Set quote and comment syntax between START and END from the parse tree.
`syntax-propertize' has already cleared the region, so only characters
whose meaning differs from the syntax table's need a property."
  (goto-char start)
  (while (re-search-forward "[\"#]" end t)
    (let* ((pos (match-beginning 0))
           (syntax (if (eq (char-after pos) ?\")
                       (tcl-ts-mode--quote-syntax pos)
                     (tcl-ts-mode--hash-syntax pos))))
      (when syntax
        (put-text-property pos (1+ pos) 'syntax-table syntax)))))


;;; Imenu and navigation.

(defun tcl-ts-mode--proc-command-p (node)
  "Return non-nil if NODE is a command from `tcl-proc-list'.
This catches the defining commands that the grammar has no dedicated
rule for, such as \"method\" or \"itcl_class\"."
  (and (equal (treesit-node-type node) "command")
       (let ((name (treesit-node-child-by-field-name node "name")))
         (and name (member (treesit-node-text name t) tcl-proc-list)))))

(defun tcl-ts-mode--defun-p (node)
  "Return non-nil if NODE is a proc, or a proc-like command."
  (or (equal (treesit-node-type node) "procedure")
      (tcl-ts-mode--proc-command-p node)))

(defun tcl-ts-mode--defun-name (node)
  "Return the name of the thing defined by NODE, or nil."
  (pcase (treesit-node-type node)
    ("procedure"
     (treesit-node-text
      (treesit-node-child-by-field-name node "name") t))
    ("command"
     (when-let* ((args (treesit-node-child-by-field-name node "arguments"))
                 (first (treesit-node-child args 0 t)))
       (treesit-node-text first t)))))


;;; Mode.

;;;###autoload
(define-derived-mode tcl-ts-mode tcl-mode "Tcl[ts]"
  "Major mode for editing Tcl code, powered by tree-sitter.

This is `tcl-mode' with fontification, Imenu and
`add-log-current-defun' supplied by the tree-sitter `tcl' grammar.
Indentation, defun navigation, electric characters and the
`inferior-tcl' commands are inherited unchanged, so every variable
documented in `tcl-mode' still applies.

Use `treesit-font-lock-level' to control how much is highlighted;
level 3, the default, is closest to `tcl-mode'.

If the `tcl' grammar is not installed, this mode is simply
`tcl-mode'.  Install the grammar with

    \\[treesit-install-language-grammar] RET tcl RET

\\{tcl-ts-mode-map}"
  (when (treesit-ready-p 'tcl)
    (treesit-parser-create 'tcl)

    ;; `tcl-indent-line', the electric keys, `forward-sexp' and
    ;; `show-paren-mode' all read the syntax table rather than the parse
    ;; tree, so replace tcl.el's regexp scanner with one driven by the
    ;; tree.  `tcl-mode's `syntax-propertize-multiline' hook stays on
    ;; `syntax-propertize-extend-region-functions': the fallback path
    ;; still goes through `tcl--syntax-of-quote', which uses it.
    (setq-local syntax-propertize-function
                #'tcl-ts-mode--syntax-propertize)

    (setq-local treesit-font-lock-settings (tcl-ts-mode--font-lock-settings))
    (setq-local treesit-font-lock-feature-list
                '((comment definition)
                  (keyword string type)
                  (builtin constant number variable)
                  (function escape-sequence operator bracket delimiter
                            misc-punctuation)))

    (setq-local treesit-defun-name-function #'tcl-ts-mode--defun-name)

    (setq-local treesit-simple-imenu-settings
                `(("Proc" ,(rx bos "procedure" eos) nil nil)
                  ("Definition" ,(rx bos "command" eos)
                   tcl-ts-mode--proc-command-p nil)))

    ;; `treesit-major-mode-setup' leaves `indent-line-function' alone as
    ;; long as `treesit-simple-indent-rules' is nil, which is what we
    ;; want: `tcl-indent-line' stays in charge.
    (treesit-major-mode-setup)

    ;; Deliberately set *after* `treesit-major-mode-setup', so that it
    ;; does not install `treesit-beginning-of-defun' as
    ;; `beginning-of-defun-function'.  `tcl-calculate-indent' and
    ;; `tcl-in-comment' call `beginning-of-defun' to choose the position
    ;; they start parsing from, and expect tcl.el's line-oriented notion
    ;; of a defun; `treesit-beginning-of-defun' can land inside a brace
    ;; group, which changes the computed indentation.  Setting it here
    ;; still gives `treesit-defun-at-point' -- and therefore
    ;; `add-log-current-defun' -- the parse tree to work with.
    (setq-local treesit-defun-type-regexp
                (cons (rx bos (or "procedure" "command") eos)
                      #'tcl-ts-mode--defun-p))
    (setq-local add-log-current-defun-function
                #'treesit-add-log-current-defun)))

(when (treesit-ready-p 'tcl t)
  (add-to-list 'major-mode-remap-alist '(tcl-mode . tcl-ts-mode)))

(provide 'tcl-ts-mode)

;;; tcl-ts-mode.el ends here
