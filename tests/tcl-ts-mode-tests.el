;;; tcl-ts-mode-tests.el --- ERT tests for tcl-ts-mode  -*- lexical-binding: t; -*-

;;; Commentary:

;; Run from the repository root:
;;
;;   emacs -Q --batch -L . -l tests/tcl-ts-mode-tests.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; Most tests need the `tcl' tree-sitter grammar and are skipped without
;; it.  If the grammar is not installed where Emacs looks by default,
;; point TCL_TS_GRAMMAR_DIR at the directory holding
;; libtree-sitter-tcl.so:
;;
;;   TCL_TS_GRAMMAR_DIR=/path/to/dir emacs -Q --batch ...

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'treesit)

;; Must happen before loading the mode, which checks for the grammar at
;; load time to decide on the `major-mode-remap-alist' entry.
(when-let* ((dir (getenv "TCL_TS_GRAMMAR_DIR")))
  (add-to-list 'treesit-extra-load-path dir))

(require 'tcl-ts-mode)

(defun tcl-ts-mode-tests--grammar-p ()
  "Return non-nil if the `tcl' grammar is available."
  (treesit-ready-p 'tcl t))

(defmacro tcl-ts-mode-tests--with-buffer (text &rest body)
  "Run BODY in a fontified `tcl-ts-mode' buffer containing TEXT.
Point starts at the beginning of the buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (insert ,text)
     (let ((major-mode-remap-alist nil))
       (tcl-ts-mode))
     (setq-local indent-tabs-mode nil)
     (font-lock-ensure)
     (goto-char (point-min))
     ,@body))

(defun tcl-ts-mode-tests--face (string &optional offset)
  "Return the face at the first occurrence of STRING, plus OFFSET."
  (save-excursion
    (goto-char (point-min))
    (unless (search-forward string nil t)
      (error "Test text does not contain %S" string))
    (get-text-property (+ (match-beginning 0) (or offset 0)) 'face)))

(defun tcl-ts-mode-tests--depth ()
  "Return the bracket depth at the end of the buffer."
  ;; `syntax-ppss' moves point to its argument.
  (save-excursion (nth 0 (syntax-ppss (point-max)))))

(defun tcl-ts-mode-tests--in-string-p (string)
  "Return non-nil if the first occurrence of STRING is inside a string."
  (save-excursion
    (goto-char (point-min))
    (search-forward string)
    (nth 3 (syntax-ppss (match-beginning 0)))))

(defun tcl-ts-mode-tests--in-comment-p (string)
  "Return non-nil if the character after STRING's start is in a comment."
  (save-excursion
    (goto-char (point-min))
    (search-forward string)
    (nth 4 (syntax-ppss (1+ (match-beginning 0))))))

(defun tcl-ts-mode-tests--indentation-of (string)
  "Return the indentation of the line containing STRING."
  (save-excursion
    (goto-char (point-min))
    (search-forward string)
    (current-indentation)))

(defconst tcl-ts-mode-tests--vivado
  "proc checkRequiredFiles { origin_dir} {
set status true
set files [list \\
\"[file normalize \"$origin_dir/IP/clk_wiz_0.xci\"]\"\\
\"[file normalize \"$origin_dir/IP/clk_wiz_1.xci\"]\"\\
]
foreach ifile $files {
if { ![file isfile $ifile] } {
puts \" Could not find local file $ifile \"
set status false
}
}
return $status
}
"
  "A Vivado-generated procedure, with all indentation stripped.")


;;; Font lock.

(ert-deftest tcl-ts-mode-test-font-lock-basics ()
  (skip-unless (tcl-ts-mode-tests--grammar-p))
  (tcl-ts-mode-tests--with-buffer
      "# a comment
proc greet {name {greeting hi}} {
    global counter
    set msg \"hello\"
    if {$name eq \"\"} { return 0 }
    puts [expr {1 + 2}]
}
set flag true
"
    (should (eq (tcl-ts-mode-tests--face "# a comment") 'font-lock-comment-face))
    (should (eq (tcl-ts-mode-tests--face "proc") 'font-lock-keyword-face))
    (should (eq (tcl-ts-mode-tests--face "greet") 'font-lock-function-name-face))
    (should (eq (tcl-ts-mode-tests--face "name {") 'font-lock-variable-name-face))
    (should (eq (tcl-ts-mode-tests--face "greeting") 'font-lock-variable-name-face))
    (should (eq (tcl-ts-mode-tests--face "global") 'font-lock-type-face))
    (should (eq (tcl-ts-mode-tests--face "msg") 'font-lock-variable-name-face))
    (should (eq (tcl-ts-mode-tests--face "\"hello\"") 'font-lock-string-face))
    (should (eq (tcl-ts-mode-tests--face "if") 'font-lock-keyword-face))
    (should (eq (tcl-ts-mode-tests--face "$name") 'font-lock-variable-use-face))
    (should (eq (tcl-ts-mode-tests--face "puts") 'font-lock-builtin-face))
    (should (eq (tcl-ts-mode-tests--face "1 +") 'font-lock-number-face))
    (should (eq (tcl-ts-mode-tests--face "true") 'font-lock-constant-face))))

(ert-deftest tcl-ts-mode-test-font-lock-keyword-lists-are-anchored ()
  "Keyword lists must match whole command names, and nothing more.
Regression test: a mangled end-of-string anchor once made every
`:match' predicate fail, so `return' and `true' lost their faces."
  (skip-unless (tcl-ts-mode-tests--grammar-p))
  (tcl-ts-mode-tests--with-buffer
      "return 1\nreturnx 1\nputs a\nputsx a\nset a true\nset b trueish\n"
    (should (eq (tcl-ts-mode-tests--face "return 1") 'font-lock-keyword-face))
    (should-not (eq (tcl-ts-mode-tests--face "returnx") 'font-lock-keyword-face))
    (should (eq (tcl-ts-mode-tests--face "puts a") 'font-lock-builtin-face))
    (should-not (eq (tcl-ts-mode-tests--face "putsx") 'font-lock-builtin-face))
    (should (eq (tcl-ts-mode-tests--face "true\n") 'font-lock-constant-face))
    (should-not (eq (tcl-ts-mode-tests--face "trueish") 'font-lock-constant-face))))

(ert-deftest tcl-ts-mode-test-font-lock-booleans-are-only-true-false ()
  (skip-unless (tcl-ts-mode-tests--grammar-p))
  (tcl-ts-mode-tests--with-buffer "set a false\nset b on\nset c no\n"
    (should (eq (tcl-ts-mode-tests--face "false") 'font-lock-constant-face))
    (should-not (tcl-ts-mode-tests--face "on"))
    (should-not (tcl-ts-mode-tests--face "no"))))

(ert-deftest tcl-ts-mode-test-font-lock-level-4 ()
  (skip-unless (tcl-ts-mode-tests--grammar-p))
  (let ((treesit-font-lock-level 4))
    (tcl-ts-mode-tests--with-buffer
        "myproc {*}$args\nif {$a && $b} { puts \"x\\ty\" }\n"
      (should (eq (tcl-ts-mode-tests--face "myproc") 'font-lock-function-call-face))
      (should (eq (tcl-ts-mode-tests--face "{*}") 'font-lock-misc-punctuation-face))
      (should (eq (tcl-ts-mode-tests--face "&&") 'font-lock-operator-face))
      (should (eq (tcl-ts-mode-tests--face "\\t") 'font-lock-escape-face)))))


;;; Code inside strings.

(defconst tcl-ts-mode-tests--string-code
  "puts \"size: [file size $f] bytes, owner $user\"
set g \"[file normalize \"$dir/x\"]\"
"
  "Strings containing command and variable substitutions.")

(ert-deftest tcl-ts-mode-test-string-code-highlighted ()
  (skip-unless (tcl-ts-mode-tests--grammar-p))
  (let ((tcl-ts-mode-highlight-string-commands t)
        (tcl-ts-mode-highlight-string-variables t))
    (tcl-ts-mode-tests--with-buffer tcl-ts-mode-tests--string-code
      ;; Literal text and the outer quotes stay string.
      (should (eq (tcl-ts-mode-tests--face "\"size") 'font-lock-string-face))
      (should (eq (tcl-ts-mode-tests--face "size:") 'font-lock-string-face))
      (should (eq (tcl-ts-mode-tests--face "bytes") 'font-lock-string-face))
      (should (eq (tcl-ts-mode-tests--face "$user\"" 5) 'font-lock-string-face))
      ;; Code inside the substitution is code.
      (should (eq (tcl-ts-mode-tests--face "file size") 'font-lock-builtin-face))
      (should (eq (tcl-ts-mode-tests--face "$f]") 'font-lock-variable-use-face))
      (should (eq (tcl-ts-mode-tests--face "$user") 'font-lock-variable-use-face))
      ;; A string nested inside a substitution is a string again...
      (should (eq (tcl-ts-mode-tests--face "\"$dir") 'font-lock-string-face))
      (should (eq (tcl-ts-mode-tests--face "/x\"") 'font-lock-string-face))
      ;; ...with its own variables highlighted.
      (should (eq (tcl-ts-mode-tests--face "$dir") 'font-lock-variable-use-face)))))

(ert-deftest tcl-ts-mode-test-string-commands-off ()
  (skip-unless (tcl-ts-mode-tests--grammar-p))
  (let ((tcl-ts-mode-highlight-string-commands nil)
        (tcl-ts-mode-highlight-string-variables t))
    (tcl-ts-mode-tests--with-buffer tcl-ts-mode-tests--string-code
      (should (eq (tcl-ts-mode-tests--face "file size") 'font-lock-string-face))
      (should (eq (tcl-ts-mode-tests--face "$f]") 'font-lock-string-face))
      ;; Variables directly in the string are still highlighted.
      (should (eq (tcl-ts-mode-tests--face "$user") 'font-lock-variable-use-face)))))

(ert-deftest tcl-ts-mode-test-string-variables-off ()
  (skip-unless (tcl-ts-mode-tests--grammar-p))
  (let ((tcl-ts-mode-highlight-string-commands t)
        (tcl-ts-mode-highlight-string-variables nil))
    (tcl-ts-mode-tests--with-buffer tcl-ts-mode-tests--string-code
      (should (eq (tcl-ts-mode-tests--face "$user") 'font-lock-string-face))
      ;; Code in a command substitution is not a string variable, so it
      ;; is still highlighted.
      (should (eq (tcl-ts-mode-tests--face "file size") 'font-lock-builtin-face))
      (should (eq (tcl-ts-mode-tests--face "$f]") 'font-lock-variable-use-face)))))

(ert-deftest tcl-ts-mode-test-string-code-both-off-is-all-string ()
  (skip-unless (tcl-ts-mode-tests--grammar-p))
  (let ((tcl-ts-mode-highlight-string-commands nil)
        (tcl-ts-mode-highlight-string-variables nil))
    (tcl-ts-mode-tests--with-buffer tcl-ts-mode-tests--string-code
      (let ((line-end (save-excursion (goto-char (point-min)) (line-end-position))))
        (should (cl-loop for pos from (+ (point-min) 5) below line-end
                         always (eq (get-text-property pos 'face)
                                    'font-lock-string-face)))))))

(ert-deftest tcl-ts-mode-test-string-code-no-gap-at-low-level ()
  "Below level 3 the code rules are off, so nothing should be left unfaced."
  (skip-unless (tcl-ts-mode-tests--grammar-p))
  (let ((treesit-font-lock-level 2)
        (tcl-ts-mode-highlight-string-commands t)
        (tcl-ts-mode-highlight-string-variables t))
    (tcl-ts-mode-tests--with-buffer tcl-ts-mode-tests--string-code
      (should (eq (tcl-ts-mode-tests--face "file size") 'font-lock-string-face))
      (should (eq (tcl-ts-mode-tests--face "$user") 'font-lock-string-face)))))

(ert-deftest tcl-ts-mode-test-string-options-apply-on-refontify ()
  (skip-unless (tcl-ts-mode-tests--grammar-p))
  (tcl-ts-mode-tests--with-buffer tcl-ts-mode-tests--string-code
    (setq-local tcl-ts-mode-highlight-string-variables t)
    (font-lock-flush)
    (font-lock-ensure)
    (should (eq (tcl-ts-mode-tests--face "$user") 'font-lock-variable-use-face))
    (setq-local tcl-ts-mode-highlight-string-variables nil)
    (font-lock-flush)
    (font-lock-ensure)
    (should (eq (tcl-ts-mode-tests--face "$user") 'font-lock-string-face))))


;;; Syntax: quotes, brackets and comments.

(ert-deftest tcl-ts-mode-test-syntax-nested-substitution ()
  "The motivating case: tcl.el leaves the bracket depth at -1 here."
  (skip-unless (tcl-ts-mode-tests--grammar-p))
  (tcl-ts-mode-tests--with-buffer
      "set f \"[file normalize \"/tmp/x\"]\"\nputs one\n"
    (should (= (tcl-ts-mode-tests--depth) 0))
    (should-not (tcl-ts-mode-tests--in-string-p "puts one"))
    ;; forward-sexp from the `[' reaches just past the matching `]'.
    (search-forward "[")
    (backward-char)
    (forward-sexp)
    (should (eq (char-before) ?\]))
    (should (looking-at-p "\"\n"))))

(ert-deftest tcl-ts-mode-test-patch-fixes-plain-tcl-mode ()
  "Loading tcl-ts-mode applies the tcl.el patch, so plain `tcl-mode' is fixed.
Needs no grammar."
  (with-temp-buffer
    (insert "set f \"[file normalize \"/tmp/x\"]\"\nputs one\n")
    (let ((major-mode-remap-alist nil))
      (tcl-mode))
    (should (= (tcl-ts-mode-tests--depth) 0))
    (should-not (tcl-ts-mode-tests--in-string-p "puts one"))))

(ert-deftest tcl-ts-mode-test-syntax-vivado-list ()
  (skip-unless (tcl-ts-mode-tests--grammar-p))
  (tcl-ts-mode-tests--with-buffer tcl-ts-mode-tests--vivado
    (should (= (tcl-ts-mode-tests--depth) 0))
    (should-not (tcl-ts-mode-tests--in-string-p "foreach"))))

(ert-deftest tcl-ts-mode-test-syntax-no-runaway-strings ()
  "Unpaired or invented quotes must not open a string for the whole file.
Each of these once broke the rest of the buffer at some point during
development; tcl.el gets all of them right."
  (skip-unless (tcl-ts-mode-tests--grammar-p))
  (dolist (text '("regexp {a\"b} $x\n"
                  "puts {unmatched \" }\n"
                  "set s {it's \"x\" ok}\n"
                  "regexp {a\"b\"c} $x\n"
                  "puts \"unmatched [ here\"\n"
                  "set m [regexp -all -inline {\\+incdir\\+\"[^\"]+\"} $line]\n"
                  "if {[regexp {\\+incdir\\+\"([^\"]+)\"} $line]} {\n}\n"
                  "set x {a \"b c}\n"))
    (tcl-ts-mode-tests--with-buffer (concat text "proc foo {} { puts hello }\n")
      (ert-info ((format "Text: %S" text))
        (should (= (tcl-ts-mode-tests--depth) 0))
        (should-not (tcl-ts-mode-tests--in-string-p "proc foo"))))))

(ert-deftest tcl-ts-mode-test-syntax-plain-strings ()
  (skip-unless (tcl-ts-mode-tests--grammar-p))
  (tcl-ts-mode-tests--with-buffer
      "puts \"hello world\"\nputs \"he said \\\"hi\\\"\"\nputs \"line one\nline two\"\nputs done\n"
    (should (tcl-ts-mode-tests--in-string-p "world"))
    (should (tcl-ts-mode-tests--in-string-p "hi\\"))
    (should (tcl-ts-mode-tests--in-string-p "line two"))
    (should-not (tcl-ts-mode-tests--in-string-p "puts done"))
    (should (= (tcl-ts-mode-tests--depth) 0))))

(ert-deftest tcl-ts-mode-test-syntax-comments ()
  (skip-unless (tcl-ts-mode-tests--grammar-p))
  (tcl-ts-mode-tests--with-buffer
      "# a } brace and \"quote\nputs abc#def\nputs #foo\nset colour #ff0000\nputs x ;# trailing\nputs done\n"
    (should (tcl-ts-mode-tests--in-comment-p "# a }"))
    ;; The quote inside the comment must not open a string.
    (should-not (tcl-ts-mode-tests--in-string-p "puts abc"))
    ;; A `#' that does not start a command is not a comment.
    (should-not (tcl-ts-mode-tests--in-comment-p "#def"))
    (should-not (tcl-ts-mode-tests--in-comment-p "#foo"))
    (should-not (tcl-ts-mode-tests--in-comment-p "#ff0000"))
    (should (tcl-ts-mode-tests--in-comment-p "# trailing"))
    (should (= (tcl-ts-mode-tests--depth) 0))))


;;; Indentation.

(ert-deftest tcl-ts-mode-test-indent-vivado ()
  "The stock tcl.el flattens everything after the nested list to column 0."
  (skip-unless (tcl-ts-mode-tests--grammar-p))
  (tcl-ts-mode-tests--with-buffer tcl-ts-mode-tests--vivado
    (indent-region (point-min) (point-max))
    (should (= (tcl-ts-mode-tests--indentation-of "set status true") 4))
    (should (= (tcl-ts-mode-tests--indentation-of "foreach") 4))
    (should (= (tcl-ts-mode-tests--indentation-of "if {") 8))
    (should (= (tcl-ts-mode-tests--indentation-of "Could not") 12))
    (should (= (tcl-ts-mode-tests--indentation-of "return $status") 4))
    (should (= (tcl-ts-mode-tests--indentation-of "clk_wiz_0")
               (tcl-ts-mode-tests--indentation-of "clk_wiz_1")))))

(ert-deftest tcl-ts-mode-test-indent-matches-tcl-mode ()
  "On code tcl.el lexes correctly, indentation is exactly `tcl-mode's."
  (skip-unless (tcl-ts-mode-tests--grammar-p))
  (let* ((text "namespace eval ::demo {
variable counter 0
proc greet {name} {
global counter
if {$name ne \"\"} {
puts \"hi $name\"
} elseif {$counter > 1} {
puts \\
    continued
} else {
error \"no name\"
}
foreach i [list a b] {
lappend out $i
}
}
}
")
         (indent (lambda (mode)
                   (with-temp-buffer
                     (insert text)
                     (let ((major-mode-remap-alist nil)) (funcall mode))
                     (setq-local indent-tabs-mode nil)
                     (indent-region (point-min) (point-max))
                     (buffer-string)))))
    (should (equal (funcall indent #'tcl-ts-mode) (funcall indent #'tcl-mode)))))


;;; Behaviour inherited from, or kept compatible with, tcl.el.

(ert-deftest tcl-ts-mode-test-keeps-tcl-el-indent-and-navigation ()
  "Treesit defun navigation would break `tcl-calculate-indent'."
  (skip-unless (tcl-ts-mode-tests--grammar-p))
  (tcl-ts-mode-tests--with-buffer "proc a {} {}\n"
    (should (eq indent-line-function #'tcl-indent-line))
    (should (eq comment-indent-function #'tcl-comment-indent))
    (should (null beginning-of-defun-function))
    (should (eq end-of-defun-function #'tcl-end-of-defun-function))
    (should (derived-mode-p 'tcl-mode))))

(ert-deftest tcl-ts-mode-test-imenu ()
  (skip-unless (tcl-ts-mode-tests--grammar-p))
  (tcl-ts-mode-tests--with-buffer
      "proc top {} {}\nnamespace eval ::ns {\n    proc ::ns::inner {} {}\n}\nmethod frob {x} {}\n"
    (let ((index (funcall imenu-create-index-function)))
      (should (equal (mapcar #'car (cdr (assoc "Proc" index)))
                     '("top" "::ns::inner")))
      (should (equal (mapcar #'car (cdr (assoc "Definition" index)))
                     '("frob"))))))

(ert-deftest tcl-ts-mode-test-add-log-current-defun ()
  (skip-unless (tcl-ts-mode-tests--grammar-p))
  (tcl-ts-mode-tests--with-buffer
      "proc ::demo::greet {name} {\n    puts $name\n}\nputs outside\n"
    (search-forward "puts $name")
    (should (equal (add-log-current-defun) "::demo::greet"))))

(ert-deftest tcl-ts-mode-test-remap ()
  (skip-unless (tcl-ts-mode-tests--grammar-p))
  (should (eq (alist-get 'tcl-mode major-mode-remap-alist) 'tcl-ts-mode)))

(ert-deftest tcl-ts-mode-test-without-grammar-is-tcl-mode ()
  "Without the grammar the mode must degrade to plain `tcl-mode'."
  (with-temp-buffer
    (insert "proc a {} { puts \"x\" }\n")
    (cl-letf (((symbol-function 'treesit-ready-p) (lambda (&rest _) nil)))
      (let ((major-mode-remap-alist nil))
        (tcl-ts-mode)))
    (should (eq major-mode 'tcl-ts-mode))
    (should (null (treesit-parser-list)))
    (should (eq (car font-lock-defaults) 'tcl-font-lock-keywords))
    (should (eq syntax-propertize-function tcl-syntax-propertize-function))
    (should (eq indent-line-function #'tcl-indent-line))))

(provide 'tcl-ts-mode-tests)

;;; tcl-ts-mode-tests.el ends here
