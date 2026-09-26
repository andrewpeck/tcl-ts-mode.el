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
;; and the `inferior-tcl' commands all behave exactly as before.  Only
;; fontification, Imenu and `add-log-current-defun' are replaced by
;; tree-sitter equivalents.
;;
;; Loading this file also applies tcl-ts-mode-tcl-patch.el, which fixes
;; tcl.el's handling of a command substitution nested inside a string,
;; as Vivado and similar generators emit constantly:
;;
;;   set files [list \
;;    "[file normalize "$origin_dir/IP/clk_wiz_0.xci"]"\
;;    ]
;;
;; Unpatched, tcl.el mis-lexes that, its bracket depth goes negative, and
;; every following line in the file loses its indentation.  The patch
;; changes plain `tcl-mode' too.
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
;; Indentation is `tcl-indent-line' throughout, and matches (patched)
;; `tcl-mode' exactly.
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
(require 'tcl-ts-mode-tcl-patch)

;; Until the fix is in Emacs itself.
(tcl-ts-mode-tcl-patch-apply)

(declare-function treesit-node-child "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-parser-create "treesit.c")

(add-to-list 'treesit-language-source-alist
             '(tcl "https://github.com/tree-sitter-grammars/tree-sitter-tcl"
                   "main" "src")
             t)


(defcustom tcl-ts-mode-highlight-string-commands t
  "Non-nil means highlight command substitutions inside strings as code.
With this on, in

    puts \"size: [file size $f] bytes\"

the `[file size $f]' part is fontified like any other command, and only
the literal text around it gets `font-lock-string-face'.  With it off,
the whole quoted word is a string, as in `tcl-mode'.

This only applies while the `builtin' font-lock feature is enabled,
which by default is from `treesit-font-lock-level' 3; below that the
substitution keeps the string face rather than showing unfontified.

Takes effect the next time the buffer is fontified; use
\\[font-lock-update] to see a change straight away."
  :type 'boolean
  :safe #'booleanp
  :group 'tcl)

(defcustom tcl-ts-mode-highlight-string-variables t
  "Non-nil means highlight variable substitutions inside strings.
With this on, in

    puts \"hello $name\"

`$name' gets `font-lock-variable-use-face' and only the literal text
around it gets `font-lock-string-face'.  With it off, the variable is
part of the string, as in `tcl-mode'.

This only applies while the `variable' font-lock feature is enabled,
which by default is from `treesit-font-lock-level' 3; below that the
variable keeps the string face rather than showing unfontified.

Takes effect the next time the buffer is fontified; use
\\[font-lock-update] to see a change straight away."
  :type 'boolean
  :safe #'booleanp
  :group 'tcl)


;;; Font lock.

(defun tcl-ts-mode--feature-enabled-p (feature)
  "Return non-nil if the font-lock FEATURE is enabled in this buffer."
  (let (on)
    (dolist (setting treesit-font-lock-settings on)
      (when (and (eq (nth 2 setting) feature) (nth 1 setting))
        (setq on t)))))

(defun tcl-ts-mode--fontify-quoted-word (node override start end &rest _)
  "Give the literal parts of the quoted word NODE `font-lock-string-face'.
Command and variable substitutions directly inside NODE are skipped,
according to `tcl-ts-mode-highlight-string-commands' and
`tcl-ts-mode-highlight-string-variables', so the code rules can fontify
them.  Each is skipped only while the feature that fontifies it is
enabled, so a low `treesit-font-lock-level' never leaves a gap.  A
quoted word nested inside a command substitution is matched by the same
query and handled on its own.  OVERRIDE, START and END are as for
`treesit-fontify-with-override'."
  (let ((pos (treesit-node-start node))
        (code (append
               (and tcl-ts-mode-highlight-string-commands
                    (tcl-ts-mode--feature-enabled-p 'builtin)
                    '("command_substitution"))
               (and tcl-ts-mode-highlight-string-variables
                    (tcl-ts-mode--feature-enabled-p 'variable)
                    '("variable_substitution")))))
    (dolist (child (treesit-node-children node t))
      (when (member (treesit-node-type child) code)
        (treesit-fontify-with-override
         pos (treesit-node-start child) 'font-lock-string-face
         override start end)
        (setq pos (treesit-node-end child))))
    (treesit-fontify-with-override
     pos (treesit-node-end node) 'font-lock-string-face override start end)))

(defun tcl-ts-mode--anchored-opt (words)
  "Return a regexp matching exactly any string in WORDS."
  (concat "\\`" (regexp-opt words) "\\'"))

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
   '((quoted_word) @tcl-ts-mode--fontify-quoted-word)

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
auto_path\\|tcl_[a-zA-Z]+\\)\\'"
              @font-lock-builtin-face)))

   :language 'tcl
   :feature 'constant
   ;; Deliberately not "yes"/"no"/"on"/"off": Tcl accepts them as
   ;; booleans, but they are far more often ordinary argument words.
   '(((simple_word) @font-lock-constant-face
      (:match "\\`\\(?:true\\|false\\)\\'" @font-lock-constant-face)))

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
