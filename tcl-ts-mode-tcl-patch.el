;;; tcl-ts-mode-tcl-patch.el --- Fix tcl.el quote syntax in command substitutions  -*- lexical-binding: t; -*-

;; Author: Andrew Peck <me@andrewpeck.xyz>

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

;; A monkey-patch for tcl.el, pending the same fix upstream.
;;
;; tcl.el mis-lexes a quoted word inside a command substitution inside a
;; string, which Vivado and similar generators emit constantly:
;;
;;   set f "[file normalize "$dir/x.xci"]"
;;
;; It closes the outer string at the inner opening quote and demotes the
;; inner closing quote, so the `]' counts as live code, the bracket depth
;; goes negative, and every following line loses its indentation.  This
;; is the FIXME above `tcl-syntax-propertize-function' in tcl.el.
;;
;; `tcl-ts-mode-tcl-patch-apply' redefines `tcl--syntax-of-quote' so that
;; a quote inside a `[...]' within a string is punctuation rather than a
;; string delimiter, and adds the two helpers it needs.  It changes plain
;; `tcl-mode' as well as `tcl-ts-mode'.
;;
;; The functions are as proposed upstream, except that `incf' and `decf'
;; are spelled `cl-incf' and `cl-decf': the unprefixed names only exist
;; from Emacs 31.

;;; Code:

(require 'cl-lib)
;; Load tcl.el first, so that it cannot later overwrite the patch.
(require 'tcl)

;; Defined by `tcl-ts-mode-tcl-patch-apply'.
(declare-function tcl--scan-brackets "tcl-ts-mode-tcl-patch")
(declare-function tcl--quote-within-command-p "tcl-ts-mode-tcl-patch")

(defun tcl-ts-mode-tcl-patch-apply ()
  "Replace tcl.el's quote syntax with one that handles `[...]' in strings."

  (defun tcl--scan-brackets (depth bound stop-at-zero)
    "Scan from point to BOUND, adjusting DEPTH for every `[' and `]' found.
Stop as soon as DEPTH is back to zero if STOP-AT-ZERO is non-nil.
Backslash escapes are skipped.  Return the resulting DEPTH and leave
point where the scan stopped."
    (catch 'done
      (while (re-search-forward "[][\\]" bound t)
        (pcase (char-before)
          ;; handle escapes so we don't count \[ or \] by skipping past
          ;; them but don't skip out of our bounded region
          (?\\ (when (< (point)
                        (or bound (point-max)))
                 (forward-char 1)))
          ;; open brackets
          (?\[ (cl-incf depth))
          ;; close brackets
          (?\] (unless (zerop depth) (cl-decf depth))))
        (when (and stop-at-zero
                   (zerop depth))
          (throw 'done nil))))
    depth)

  (defun tcl--quote-within-command-p (pos string-start)
    "Return t if POS is inside a `[...]' command substitution.

STRING-START is the position of the quote that opened the string
in which POS appears."
    (save-excursion
      (goto-char (1+ string-start))
      ;; scan from the start of the string to figure out bracket depth at pos
      ;; (how many brackets we have open)
      (let ((depth (tcl--scan-brackets 0 pos nil)))
        (when (> depth 0)
          ;; scan until we find the end of the command and ensure that
          ;; brackets are balanced in this quoted string
          (setq depth (tcl--scan-brackets depth nil t))
          ;; The closing `]' is not on the same line as the quote so apply
          ;; a syntax multiline property to ensure that a
          ;; a stray `[' does not turn the rest of the buffer into a string.
          (when (> (line-beginning-position) pos)
            (put-text-property pos (point) 'syntax-multiline t))
          (zerop depth)))))

  (defun tcl--syntax-of-quote (pos)
    "Decide whether a double quote opens a string or not."
    ;; This is pretty tricky, because strings can be written as "..."
    ;; or as {...} or without any quoting at all for some simple and not so
    ;; simple cases (e.g. `abc' but also `a"b').  To make things more
    ;; interesting, code is represented as strings, so the content of
    ;; strings can be later re-lexed to find nested strings.
    (save-excursion
      (let ((ppss (syntax-ppss pos)))
        (cond
         ((nth 8 ppss) ;; Within a string or a comment.
          ;; tcl forbids quotes inside of a quote, e.g.
          ;; set a "some string "with a substring""
          ;; but *does* allow quotes inside of a command in quotes, e.g.
          ;; "some [command "with some arg"]"
          ;;
          ;; when inside of a command in a string, treat quotes as syntax
          ;; rather than closing the opening string
          ;;
          ;; "some [command "with some arg"]"
          ;;                ^ this should not close the first quote
          (when (and (nth 3 ppss) ;; Within a string.
                     ;; Within a command [...].
                     (tcl--quote-within-command-p pos (nth 8 ppss)))
            (string-to-syntax ".")))
         ((not (memq (char-before pos)
                     (cons nil
                           (eval-when-compile
                             (mapcar #'identity tcl--word-delimiters)))))
          ;; The double quote appears within some other lexical entity.
          ;; FIXME: Similar treatment should be used for `{' which can appear
          ;; within non-delimited strings (but only at top-level, so
          ;; maybe it's not worth worrying about).
          (string-to-syntax "."))
         ((zerop (nth 0 ppss))
          ;; Not within a { ... }, so can't be truncated by a }.
          ;; FIXME: The syntax-table also considers () and [] as paren
          ;; delimiters just like {}, even though Tcl treats them differently.
          ;; Tho I'm not sure it's worth worrying about, either.
          nil)
         (t
          ;; A double quote within a {...}: leave it as a normal string
          ;; delimiter only if we don't find a closing } before we
          ;; find a closing ".
          (let ((type nil)
                (depth 0))
            (forward-char 1)
            (while (and (not type)
                        (re-search-forward "[\"{}\\]" nil t))
              (pcase (char-after (match-beginning 0))
                (?\\ (forward-char 1))
                (?\" (setq type 'matched))
                (?\{ (cl-incf depth))
                (?\} (if (zerop depth) (setq type 'unmatched)
                       (cl-incf depth)))))
            (when (> (line-beginning-position) pos)
              ;; The quote is not on the same line as the deciding
              ;; factor, so make sure we revisit this choice later.
              (put-text-property pos (point) 'syntax-multiline t))
            (when (eq type 'unmatched)
              ;; The quote has no matching close because a } closes the
              ;; surrounding string before, so it doesn't really "open a string".
              (string-to-syntax ".")))))))))

(provide 'tcl-ts-mode-tcl-patch)

;;; tcl-ts-mode-tcl-patch.el ends here
