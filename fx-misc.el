;;; fx-misc.el --- miscellaneous small functions/settings  -*-coding: iso-2022-7bit;-*-

;; Copyright (C) 2004, 2005, 2006, 2008, 2009  Free Software Foundation, Inc.

;; Author: Dave Love <fx@gnu.org>
;; Keywords: extensions
;; $Revision: 1.37 $
;; URL: http://www.loveshack.ukfsn.org/emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Some functions and settings from my .emacs that might be useful to
;; other people but don't merit a package.  Some of them have been
;; contributed to Emacs unsuccessfully.

;;; Code:

(eval-when-compile	 ; Actually it's probably not worth compiling.
  (autoload 'latin1-char-displayable-p "latin1-disp")
  (autoload 'latin1-display-char "latin1-disp")
  (autoload 'diff-find-source-location "diff-mode")
  (autoload 'Man-fontify-manpage "man")
  (defvar spook-phrases-file)
  (defvar mail-spool-file)
  (defvar change-log-default-name))

;;; Random functions/commands

(defun vc-add-log-entry ()
  "Add a VC log entry for the current defun.
If the current buffer is under version control and it needs a checkin,
start an entry in the VC log buffer suitable for use by
`vc-update-change-log', initiating a checkin if necessary.  Otherwise
do an `add-change-log-entry-other-window'."
  (interactive)
  (require 'add-log)
  (let* ((name (or (buffer-file-name)
		   (when (eq major-mode 'diff-mode)
		     (condition-case ()
			 (buffer-file-name (car (diff-find-source-location)))
		       (error nil)))))
	 (base (if name
		   (file-name-sans-versions name)
		 (error "Buffer not visiting a file"))))
    (save-excursion
      (if base (find-file-noselect base))
      (if (or (not (vc-backend base))	; under vc?
	      (progn 
		(require 'vc) ; for vc-workfile-unchanged-p
		(vc-workfile-unchanged-p name))) ; needs checkin?
	  (add-change-log-entry-other-window) ; use ChangeLog directly
	(let ((defun (add-log-current-defun)))
	  (vc-next-action nil)
	  (set (make-local-variable 'paragraph-start)
	       (concat paragraph-start "\\|("))	; refilling convenience
	  (goto-char (point-max))
	  (setq fill-column 71)	; rcs2log default line length - 8 (tab)
	  (if (not (bolp)) (newline))
	  (when defun
	    (undo-boundary)
	    (insert "(" defun "): ")))))))

(defcustom vc-keyword-header-length 5000
  "*Size of initial buffer region which `vc-update-version-keyword' considers.
If nil, consider the whole buffer."
  :group 'vc
  :type '(choice integer (const nil)))

(defcustom vc-keyword-header-backends '(DARCS SVN)
  "*List of backends considered by `vc-update-version-keyword'.
Expansion is only attempted if the file is version-controlled by a
backend in this list."
  :group 'vc
  :type `(repeat ,(cons 'choice (mapcar (lambda (elt) (list 'const elt))
					vc-handled-backends))))

(defun vc-update-version-keyword ()
  "Increment RCS-style Revision keywords in the buffer.
E.g. $\Revision: 1.1 $ -> $\Revision: 1.2 $,
$\Revision: 1$ -> $\Revision: 2$.

It can be useful to add this to `vc-before-checkin-hook' to update
version stamps automatically with VC backends that don't expand
keywords.  Backends which do the expansion rewrite the modified
pattern on checkin anyway, so it is harmless to put this on the hook
globally.

See also `vc-keyword-header-length', `vc-keyword-header-backends'."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char 1)
      (when (re-search-forward "[$]Revision:\\s-*\\(\\S-*\\)\\s-*[$]"
			       vc-keyword-header-length t)
	(if (save-match-data (string-match "^\\s-*$" (match-string 1)))
	    (replace-match "1" t t nil 1)
	  (let ((next (save-match-data
			(vc-default-next-version nil nil (match-string 1)))))
	    (if next (replace-match next t t nil 1))))
	(save-buffer)))))

(custom-add-option 'vc-before-checkin-hook 'vc-update-version-keyword)

(defun ediff-diff-buffer-with-saved ()
  "Run Ediff between the (modified) current buffer and the buffer's file.

A new buffer is created containing the disc file's contents and
`ediff-buffers' is run to compare that with the current buffer."
  (interactive)
  (unless (buffer-modified-p)
    (error "Buffer isn't modified"))
  (let ((current (buffer-name))
        (file (or (buffer-file-name)
                  (error "Current buffer isn't visiting a file")))
        (mode major-mode))
    (set-buffer (get-buffer-create (generate-new-buffer-name
                                    (concat current "-on-disc"))))
    (buffer-disable-undo)
    (insert-file-contents file)
    (set-buffer-modified-p nil)
    (funcall mode)
    (ediff-buffers (buffer-name) current)))

(defun kill-outer-form (&optional n)
  "Delete a level of nested sexp around the current sexp and reindent.
E.g. replace `(... (x ...) ...)' with `(x ...)'.
With optional arg N, remove N levels."
  (interactive "p")
  (let ((point (point))
	point2)
    (backward-up-list 1)
    (setq point2 (point))
    (undo-boundary)
    (kill-sexp 1)
    (backward-up-list (or n 1))
    (yank)
    (kill-sexp 1)
    (backward-sexp)
    (indent-sexp)
    (forward-char (- point point2))))

(defun gethostaddr (name-or-number)
  "Return IP host name or address corresponding to NAME-OR-NUMBER using host(1).
If the arg is an IP number, return the host name, else return the address
corresponding to the host name arg."
  (if (string-match "^[[:digit:]]+\\(\\.[[:digit:]]+\\)\\{3\\}$"
		    name-or-number)
      name-or-number
    (let ((str (shell-command-to-string
		(concat "host " (shell-quote-argument name-or-number)))))
      (string-match "\\s-+\\([[:digit:]]+\\(\\.[[:digit:]]+\\)\\{3\\}\\)$" str)
      (match-string 1 str))))

(defun recode-coding-region (beg end coding-system)
  "Re-code the region BEG to END using the given CODING-SYSTEM.
First check that CODING-SYSTEM can encode the region.  Then encode it
and decode it using CODING-SYSTEM.

This can be useful to normalize the Emacs charset(s) for the region,
e.g. to convert from iso-8859 to mule-unicode-... (coding system
utf-8) or between the compatible subsets of Latin-1 and Latin-9.  The
latter requires character unification to be on -- see library
`ucs-tables.el'."
  (interactive "r\nzCoding system: ")
  (unless (member (coding-system-base coding-system)
		  (find-coding-systems-region beg end))
    (error "%s can't encode the region" coding-system))
  (save-restriction
    (narrow-to-region beg end)
  (encode-coding-region (point-min) (point-max) coding-system)
  (decode-coding-region (point-min) (point-max) coding-system)))

(defun cat-fontify ()
  "Fontify nroff-style output using `Man-fontify-manpage'.
Don't change the modified status of the buffer."
  (interactive)
  (require 'man)
  (let ((modified (buffer-modified-p))
	(buffer-undo-list t)
	(inhibit-read-only t))
    (unwind-protect
	(Man-fontify-manpage)
      (unless modified
	(restore-buffer-modified-p nil)))))

(defcustom change-log-emails nil
  "*Alist of regexps and email addresses for `change-log-choose-email'.
E.g.:
  '((\"/\\(?:emacs\\|gnus\\|esrc\\|heimdal\\)\\>\" . \"fx@gnu.org\"))"
  :group 'change-log
  :type '(alist :key-type regexp :value-type string))

;; This is broken in Emacs 22 because that runs the hook at the wrong
;; time, sigh.  You can do the same job with dir-locals.el.
(defun change-log-choose-email ()
  "Set an appropriate `add-log-mailing-address' for file being edited.
For adding to `change-log-mode-hook'.
Looks for a match of the buffer file name in `change-log-emails' and sets
the corresponding address locally if there is a match.  Also returns the
address."
  (when (buffer-file-name)
    (let ((result (assoc-default (buffer-file-name) change-log-emails
				 #'string-match)))
      (if result
	  (set (make-local-variable 'add-log-mailing-address) result)))))

(custom-add-option 'change-log-mode-hook 'change-log-choose-email)

(defcustom change-log-logs nil
  "*Alist of regexps and email addresses for `change-log-choose-log'.
E.g.:
  '((\"/emacs-unicode-2\\>\" . \"ChangeLog.22\"))"
  :group 'change-log
  :type '(alist :key-type regexp :value-type string))

(defun change-log-choose-log ()
  "Set an appropriate `change-log-default-name' for file being edited.
For adding to `find-file-hooks' (or `find-file-hook' in Emacs 22).
Looks for a match of the buffer file name in `change-log-logs' and sets
the corresponding file name locally if there is a match.  Also returns the
name."
  (when (buffer-file-name)
    (let ((result (assoc-default (buffer-file-name) change-log-logs
				 #'string-match)))
      (if result
	  (set (make-local-variable 'change-log-default-name) result)))))

(custom-add-option 'find-file-hooks 'change-log-choose-log)
(custom-add-option 'find-file-hook 'change-log-choose-log)

(defun my-display-time-mail-function ()
  "Check for mail in the spool with an external command.
For use as `display-time-mail-function'.
Using an external command allows it to be interrupted if the spool goes
away.  The default version can just hang when the NFS server dies."
   (= 0 (call-process "test" nil nil nil "-s"
		      (or (getenv "MAIL") mail-spool-file))))

(defun function-arity (function)
  "Return information on the arity (argument numbers) of FUNCTION.
The result is of the form returned by `subr-arity' or the symbol
`unknown' for an autoloaded function (whose arity is unknown).

FUNCTION must be a function (or special form) according to
`functionp', or else a macro."
  (setq function (indirect-function function))
  (cond ((eq 'autoload (car-safe function))
	 'unknown)
	((subrp function)
	 (subr-arity function))
	(t				; macro, lambda or byte code
	 (let ((min-args 0)
	       lambda-list max-args &optional)
	   (if (eq 'macro (car-safe function))
	       (pop function)		; now byte code or lambda
	     (unless (functionp function)
	       (signal 'invalid-function (list function))))
	   (if (eq 'lambda (car-safe function))
	       (setq lambda-list (cadr function))
	     (if (not (byte-code-function-p function))
		 'unknown		; shouldn't happen
	       (setq lambda-list (aref function 0))))
	   ;; We've got a lambda list.
	   (while (and lambda-list (not (eq 'many max-args)))
	     (cond ((eq (car lambda-list) '&optional)
		    (setq &optional 0))
		   ((eq (car lambda-list) '&rest)
		    (setq max-args 'many))
		   (t
		    (if &optional
			(setq &optional (1+ &optional))
		      (setq min-args (1+ min-args)))))
	     (pop lambda-list))
	   (unless max-args (setq max-args (+ min-args (or &optional 0))))
	   (cons min-args max-args)))))

(defun eval-result ()
  "Display value of previous sexp in the style of the Lisp manual.
Similar to `eval-print-last-sexp', but inserts a `=>' `prompt'."
  (interactive)
  (unless (bolp)
    (newline))
  (let ((point (point)))
    (with-syntax-table emacs-lisp-mode-syntax-table
      (eval-last-sexp t))	  ; fixme: maybe use pp-eval-last-sexp
    (save-excursion
      (goto-char point)
      (insert "  => ")))
  (newline))

;; Independently done from Noah Friedman's similar thing (for VM?)
(defun spook-boundary (count)
  "Make MIME boundary for COUNT-th call out of `spook'y phrases.
An amusing value for `mml-boundary-function'."
  (require 'spook)
  (let ((str (subst-char-in-string
	      ?\  ?\- (mapconcat (lambda (x)
				   (cookie spook-phrases-file nil nil))
				 (make-string 5 ?.)
				 "-"))))
    ;; The boundary length must be <= 70.
    (if (> (length str) 65)
	(setq str (substring str 0 65)))
    (if (>= count 6)
	(setq str (format "%s-%d" str count)))
    str))

(defvar mml-boundary-function #'spook-boundary)	; not customizable

(defun my-indent-for-tab-command (&optional beg end prefix-arg)
  "Indent active region in `transient-mark-mode' using `indent-region'.
Otherwise indent current line with `indent-for-tab-command'.  Do that
also if the region is confined to one line."
  (interactive (let* ((mark (if mark-active (mark)))
		     (beg (if mark
			      (min mark (point))
			    (point)))
		     (end (if mark (max mark (point)))))
		 (list beg end current-prefix-arg)))
  (setq this-command 'indent-for-tab-command)
  (if (not (and mark-active transient-mark-mode))
      (indent-for-tab-command prefix-arg)
    (if (= (save-excursion
	     (goto-char beg)
	     (line-beginning-position))
	   (save-excursion
	     (goto-char end)
	     (line-beginning-position)))
	(indent-for-tab-command prefix-arg)
      (indent-region beg end prefix-arg))))

(autoload 'syntax-ppss "syntax")
(autoload 'syntax-ppss-context "syntax")

(defun re-search-in-code (forward regex &optional bound)
  "Search for REGEX outside comments and strings.
FORWARD non-nil means search forward, else search backwards.
Optional BOUND bounds the search."
  (let ((function (if forward 're-search-forward 're-search-backward))
	(start (point)))
    (if (and (funcall function regex bound t)
	     (not (save-match-data (syntax-ppss-context (syntax-ppss)))))
	(match-beginning 0)
      (or (catch 'found
	    (while (funcall function regex bound t)
	      (if (not (eq 'comment
			   (syntax-ppss-context (save-match-data
						  (syntax-ppss)))))
		  (throw 'found
			 (if forward (match-end 0) (match-beginning 0)))
		(if forward
		    (unless (eobp) (forward-char))
		  (unless (bobp) (backward-char))))))
	  (progn (goto-char start)
		 nil)))))

(defvar imenu--flat-index-alist nil
  "A flattened form of `imenu--index-alist'.
Made by operating on it with `imenu--flatten-alist'.")

(make-variable-buffer-local 'imenu--flat-index-alist)

(defun imenu--flatten-alist (alist)
  "Flatten an ALIST of the form of `imenu--index-alist'.
This yields a simple alist with elements of the form
\(INDEX-NAME . INDEX-POSITION) or
\(INDEX-NAME INDEX-POSITION FUNCTION ARGUMENTS...)."
  (let (entries)
    (dolist (elt alist)
      (if (imenu--subalist-p elt)
	  (setq entries (nconc entries (imenu--flatten-alist (cdr elt))))
	(push elt entries)))
    (nreverse entries)))

(eval-when-compile
  (defvar imenu--last-index)
  (require 'imenu))

;;;###autoload
(defun imenu-all (index-item &optional after)
  "Jump to a place in the buffer chosen using a buffer menu/completion.
INDEX-ITEM specifies the position (prompted for interactively).

This is mostly like `imenu' except that if the index has multiple
levels, e.g. functions and variables, those levels are collapsed
when searching for the item.  Interactively, with prefix arg,
jump to the position of the next item (if any) in the index
matching the last name found in the buffer by this command or
\\[imenu].  E.g. in the imenu.el source, \\[imenu-all] jumps to
the defgroup, and \\[universal-argument] \\[imenu-all] jumps to the defun.
Programmatically, arg AFTER specifies the element in the list after
which to search for INDEX-ITEM."
  ;; Note that we could use `imenu--in-alist' to find the item in
  ;; `imenu--index-alist', but that doesn't allow us to find the next
  ;; one.  We need the flattened alist for that.
  (interactive (progn (unless imenu--index-alist
			(imenu--make-index-alist))
		      (if current-prefix-arg
			  (list (car-safe imenu--last-index) imenu--last-index)
			  (list (imenu-choose-buffer-index
				 nil imenu--flat-index-alist) nil))))
  (unless imenu--index-alist	       ; in case we're non-interactive
    (imenu--make-index-alist))
  (let ((alist imenu--flat-index-alist)
	(item index-item))
    (if after				; look further down the list
	(setq alist (cdr (member after alist))))
    ;; Convert a string to an alist element.
    (if (stringp item)
	(setq item (assoc item alist)))
    ;; Use the flattened alist for lookup.
    (let ((imenu--index-alist alist))
      (imenu item))
    ;; Wrap around if appropriate.
    (if (and after (null imenu--last-index))
	(imenu-all index-item))
    imenu--last-index))

(defun my-flymake-show-help ()
  "Display flymake message at point.
Useful in a tty without mouse-over.
Intended to be run from a local `post-command-hook'."
  (when (get-char-property (point) 'flymake-overlay)
    (let ((help (get-char-property (point) 'help-echo)))
      (if help (message "%s" help)))))

(add-hook 'flymake-mode-hook
	  '(lambda ()
	     (add-hook 'post-command-hook 'my-flymake-show-help nil t)))

(eval-when-compile
  (defvar nxml-prolog-end)
  (defvar xmltok-start)
  (defvar xmltok-name-end)
  (autoload 'nxml-backward-up-element "nxml-mode" nil t))

(defun nxml-which-function ()
  (save-excursion
    (let ((path nil))
      (if (<= (point) nxml-prolog-end)
	  "prolog"
	(progn
	  (while (condition-case nil
		     (unless (bobp)
		       (nxml-backward-up-element)
		       t)
		   (error nil))
	    (push (xmltok-start-tag-local-name)
		  path))
	  (mapconcat 'identity (or path '("???")) "/"))))))

;; Assume which-func-modes customized to allow nxml-mode, e.g. set to
;; t.
(add-hook 'nxml-mode-hook
	  '(lambda ()
	     (set (make-local-variable 'which-func-functions)
		  '(nxml-which-function))))

;;; Key bindings

(global-set-key [?\C-x ?\M-k] #'kill-outer-form)
(global-set-key "\C-xvA" 'vc-add-log-entry)
;; This is useful for breaking out of networking loops, e.g. Gnus
;; daemon failing to open a connexion and not obeying the timeout in
;; the background.  (This is a reported bug, unfixed as far as I know.)
(global-set-key [usr1-signal] 'top-level)
(global-set-key [?\t] 'my-indent-for-tab-command)
(define-key lisp-mode-shared-map [?\t] 'my-indent-for-tab-command)

;;; Display table fiddling

;; Extra Latin-1 substitutes in Emacs 21.
(when (bound-and-true-p latin1-display)
  (when (fboundp 'latin1-char-displayable-p) ; Emacs 21
    (unless (or (and standard-display-table ; nil in Emacs 22
		     (aref standard-display-table ?$,1r|(B))
		(latin1-char-displayable-p ?$,1r|(B))
      (dolist (elt '((?\$,1rz(B ",") ;; SINGLE LOW-9 QUOTATION MARK
		     (?\$,1r~(B ",,") ;; DOUBLE LOW-9 QUOTATION MARK
		     (?\$,1s&(B "...") ;; HORIZONTAL ELLIPSIS
		     (?\$,1s0(B "o/oo") ;; PER MILLE SIGN
		     (?\$,1s9(B "<") ;; SINGLE LEFT-POINTING ANGLE QUOTATION MARK
		     (?\$,1r|(B "``") ;; LEFT DOUBLE QUOTATION MARK
		     (?\$,1r}(B "''") ;; RIGHT DOUBLE QUOTATION MARK
		     (?\$,1rs(B "-") ;; EN DASH
		     (?\$,1rt(B "--") ;; EM DASH
		     (?\$,1ub(B "TM") ;; TRADE MARK SIGN
		     (?\$,1s:(B ">") ;; SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
		     (?$,1s"(B ",A7(B")
		     (?$,1$f(B "^")
		     (?$,1s (B "+")
		     (?$,1s!(B "++")
		     (?$,1$|(B "~")
		     (?$,1'<(B ",A5(B")
		     (?$,1rx(B "`")
		     (?$,1ry(B "'")
		     (?$,1tL(B "Euro")))
	(latin1-display-char (car elt) (cadr elt))))))

;; Kill the losing default display of eight-bit-graphic chars.
;; Display them as octal.
(when (and default-enable-multibyte-characters
	   standard-display-table
	   (/= 255 (make-char 'latin-iso8859-1 255))) ; avoid Emacs 23
  ;; This should use `map-charset-chars', but that fails for eight-bit-...
  (dotimes (i 128)
    (aset standard-display-table (+ 128 i) nil)))

;; Display Emacs-style balanced quotes properly (pace Kuhn & al).
;; Also don't treat NBSP specially if we don't have buggy fonts.
(when (and (eq window-system 'x)
	   (or (string= "The XFree86 Project, Inc" (x-server-vendor))
	       (string= "The X.Org Foundation" (x-server-vendor)))
	   (> (aref (number-to-string (nth 2 (x-server-version))) 0)
	      ?3))
  (unless standard-display-table
    (setq standard-display-table (make-display-table)))
  (aset standard-display-table ?' [?$,1ry(B])
  (aset standard-display-table ?` [?$,1rx(B])
  ;; The fonts don't have the relevant bug.
  (aset standard-display-table (make-char 'latin-iso8859-1 160) nil))

;;; Redefinitions

;; Allow `backward-up-list' to work from within strings.

(defcustom up-list-from-string-ok nil
  "*Non-nil means \\[backward-up-list] from within a string doesn't get an error.
More specifically, if point is inside a string, move to the start of
the string and move up from there.  Nil means that it will usually get
an error within a string.
Only applies interactively."
  :type 'boolean
  :group 'editing-basics)

;; Redefine to allow C-M-u to work from within strings.  Don't bother
;; with `up-list', since that doesn't have a default keybinding.
(defun backward-up-list (&optional arg interactive)
  "Move backward out of one level of parentheses.
With ARG, do this that many times.
A negative argument means move forward but still to a less deep spot.
See also `up-list' and `up-list-from-string-ok'.

Arg INTERACTIVE is for internal use."
  (interactive "p\np")
  (let ((point (point))
	(flag (when (and interactive up-list-from-string-ok)
		(let ((state (if (fboundp 'syntax-ppss)
				 (syntax-ppss) ; Emacs 22
			       (parse-partial-sexp (save-excursion
						     (beginning-of-defun)
						     (point))
						   (point)))))
		  (when (nth 3 state)
		    ;; Move out of string (when `flag' is bound non-nil).
		    (goto-char (nth 8 state)))))))
    (condition-case data
	(up-list (- (or arg 1)))
      (scan-error
       ;; Reposition if we moved out of string originally.
       (if flag
	   (goto-char point)
	 (signal 'scan-error data))))))

;; Intelligent `join-line'.

(defvar join-line-function nil
  "Function to call to join lines, or nil.
This is called by \\[delete-indentation] to do the work if it is
non-nil.  It takes an optional arg as for `delete-indentation'.

This is only expected to be set buffer-locally if the normal
behaviour isn't apprropriate for the buffer's major mode,
e.g. for fixed- or free-format Fortran.")

(eval-when-compile (autoload 'sentence-end "paragraphs"))

;; Modified to try to DTRT for comments and continuation lines.
(defun delete-indentation (&optional arg)
  "Join this line to previous and fix up whitespace at join.
If there is a fill prefix, delete it from the beginning of this line.
Similarly, if this is a comment line, delete the comment start, and
any comment end on the previous line.
If the previous line ends in an odd number of escape characters,
delete the last one, assuming this is a continuation line.
With argument, join this line to following line."
  (interactive "*P")
  ;; Arguably this should be in save-excursion.
  (if join-line-function
      (funcall join-line-function arg)
    (beginning-of-line)
    (if arg (forward-line 1))
    (when (eq (preceding-char) ?\n)
      ;; If sentence-end matches the end of the preceding line, we'll
      ;; fix up space for it later.
      (let ((sentence-end
	     (when (save-excursion
		     (re-search-backward (or sentence-end
					     (sentence-end)) ; in Emacs 22
					 (line-beginning-position 0) t))
	       ;; We've matched the preceding newline and any leading
	       ;; space on this line.
	       (>= (match-end 0) (line-beginning-position)))))
	(delete-region (1- (point)) (point))
	;; If the start of the line is between comments of some sort,
	;; join the comments.
	(let ((following-comment	; end of any following comment
	       (save-excursion (if (forward-comment 1) (point))))
	      (at-comment (looking-at "\\s-*\\s<")) ; before inline comment?
	      (preceding-comment      ; start of any preceding comment
	       (save-excursion (if (forward-comment -1) (point)))))
	  (if (and at-comment following-comment (eq ?> (char-syntax ?\n)))
	      (if (eq 'comment (syntax-ppss-context (syntax-ppss)))
		  ;; Newline-terminated comments to join.
		  (uncomment-region (point) (line-end-position))
		(when (looking-at "\\s-*\\s<\\(\\s<\\)+")
		  (replace-match "" nil nil nil 1)
		  (comment-indent)))
	    (if (and following-comment preceding-comment)
		;; Ajacent inline comments to join -- zap them and
		;; re-comment the region.
		(save-restriction
		  (narrow-to-region preceding-comment following-comment)
		  (uncomment-region (point-min) (point-max))
		  (comment-region (point-min) (point-max))))))
	;; If the second line started with the fill prefix,
	;; delete the prefix.
	(if (and fill-prefix
		 (<= (+ (point) (length fill-prefix)) (point-max))
		 (string= fill-prefix
			  (buffer-substring (point)
					    (+ (point) (length fill-prefix)))))
	    (delete-region (point) (+ (point) (length fill-prefix))))
	;; Line continuation indicated by an odd number of trailing
	;; escape chars.
	(unless (zerop (% (save-excursion (skip-syntax-backward "\\")) 2))
	  (delete-backward-char 1))
	(fixup-whitespace)
	(if (and sentence-end sentence-end-double-space)
	    (insert ?\ ))))))

;; Definition-after-use warning for macros.

(eval-after-load "bytecomp"
  '(defun byte-compile-file-form-defmacro (form)
     (when (assq (nth 1 form) byte-compile-unresolved-functions)
       (setq byte-compile-current-form (nth 1 form))
       (byte-compile-warn "macro `%s' was used before it was defined"
			  (nth 1 form)))
     (byte-compile-file-form-defmumble form t)))

;; Emacs <= 21.4 doesn't expand rx at compile time and has a single arg.

(unless (equal "" (macroexpand '(rx "")))
  (defmacro rx (&rest regexps)
    (if (cdr regexps)
	(rx-to-string `(and ,@regexps) t)
      (rx-to-string (car regexps) t))))

;; Fixes for VC functions.

(require 'vc)

;; This is a fixed version of the Emacs 22 code.
(defun vc-default-next-version (backend file rev)
  "Return the version number immediately following REV for FILE,
or nil if there is no next version.  This default implementation
works for MAJOR.MINOR-style version numbers as used by RCS
and CVS and for straight integer style used by Subversion."
  (condition-case ()		    ; don't lose if rev is tag name &c
      (let* ((branch (vc-branch-part rev))
	     (minor-num (vc-minor-part rev))
	     (minor-num (if minor-num (string-to-number minor-num))))
	(if branch
	    (concat branch "." (number-to-string (1+ minor-num)))
	  ;; subversion &c straight integers
	  (number-to-string (1+ minor-num))))
    (error)))

;; Fixed for non-RCS-style plain integers.
(defun vc-branch-part (rev)
  "Return the branch part of a revision number REV."
  (let ((index (string-match "\\.[0-9]+\\'" rev)))
    (if index
	(substring rev 0 index))))

(defun vc-minor-part (rev)
  "Return the minor version number of a revision number REV."
  (if (string-match "[0-9]+\\'" rev)
      (substring rev (match-beginning 0) (match-end 0))))

;; Adapted from Emacs 22 `vc-default-previous-version'.
(unless (fboundp 'vc-default-previous-version)
(defun vc-previous-version (rev)
  "Return the version number immediately preceding REV,
or nil if there is no previous version.  This default
implementation works for MAJOR.MINOR-style version numbers as
used by RCS and CVS and for straight integer style used by Subversion."
  (condition-case ()		    ; don't lose if rev is tag name &c
      (let* ((branch (vc-branch-part rev))
	     (minor-num (vc-minor-part rev))
	     (minor-num (if minor-num (string-to-number minor-num))))
	(if (and branch minor-num)
	    (if (> minor-num 1)
		;; version does probably not start a branch or release
		(concat branch "." (number-to-string (1- minor-num)))
	      (if (vc-trunk-p rev)
		  ;; we are at the beginning of the trunk --
		  ;; don't know anything to return here
		  nil
		;; we are at the beginning of a branch --
		;; return version of starting point
		(vc-branch-part branch)))
	  ;; subversion &c straight integers
	  (if (> minor-num 1)
	      (1- minor-num))))
    (error))))

;;; Other stuff

(eval-after-load "byte-opt"
  '(unless (fboundp 'byte-optimize-featurep)
     (put 'featurep 'byte-optimizer 'byte-optimize-featurep)
     (defun byte-optimize-featurep (form)
       ;; Emacs-21's byte-code doesn't run under XEmacs anyway, so we can
       ;; safely optimize away this test.
       (unless (equal '((quote xemacs)) (cdr-safe form))
	 form))))

;; Don't want mh-mode when recovering drafts.
(delete '("/drafts/[0-9]+\\'" . mh-letter-mode) auto-mode-alist)

(setenv "COLUMNS" "80")			; for man in 21.2

;; Not for the na,Ao(Bve...

(defalias 'yes-or-no-p 'y-or-n-p)
(define-key help-map "a" 'apropos)

(provide 'fx-misc)
;;; fx-misc.el ends here
