;;; dogescript-mode.el --- Dogescript editing mode

;; Author:           Alexandre Dantas <eu@alexdantas.net>
;; URL:              https://github.com/alexdantas/djs-mode/
;; Version:          20140114
;; Keywords:         languages, dogescript
;; Package-Requires: ((emacs "24.1"))

;;; Commentary:

;; This Dogescript editing mode supports:

;;  - syntax highlight according to the dogescript spec 2.1.0
;;  - ...that's it
;;
;; Since I'm a total noob in Emacs Lisp, I couldn't made anything
;; else.
;; If you're reading this, consider helping for a greater good :)

;; Installation:
;;
;; Place the following on your emacs config file:

;;   (add-to-list 'auto-mode-alist '("\\.djs\\'" . dogescript-mode))

;; Notes:

;; wow
;;             such dogescript-mode
;;      very elisp
;;
;;                  much parenthesis
;;    wow
;;          stahp plz

;; Thanks a lot, Xah Lee!
;; http://ergoemacs.org/emacs/elisp_syntax_coloring.html

;;; Code:

;; First, let's define a hook so users can attach code
;; when this mode is launched
(defvar dogescript-mode-map
  (let ((dogescript-mode-map (make-keymap)))
	(define-key dogescript-mode-map "\C-j" 'newline-and-indent)
	dogescript-mode-map)

  "Keymap for DOGESCRIPT major mode")

;; Defining all keywords for the mode
;; {{{
(setq dogescript-keywords
	  '("very" "is" "such" "much" "wow" "wow&" "plz"
		"with" "rly" "but" "notrly" "many" "much"
		"so" "as" "dose"))
(setq dogescript-types
	  '())
(setq dogescript-constants
	  '("dogeument" "windoge" "console"
		"true" "false"))
(setq dogescript-functions
	  '("maybe" "trained"))
;; }}}

;; Creating Regular Expressions for all the keywords above.
;; They will only match the entire words.
;; {{{
(setq dogescript-keywords-regexp  (regexp-opt dogescript-keywords  'words))
(setq dogescript-types-regexp     (regexp-opt dogescript-types     'words))
(setq dogescript-constants-regexp (regexp-opt dogescript-constants 'words))
(setq dogescript-functions-regexp (regexp-opt dogescript-functions 'words))
;; }}}

;; Creating the lists for font-lock
;; (thing that will actually color the keywords)
;; Note that the order matters!
;;
;; Xah Lee says:
;; "The `( ,a ,b …) is a lisp special syntax to evaluate parts of elements inside the list. Inside the paren, elements preceded by a , will be evaluated."
;;
;; {{{
(setq dogescript-font-lock-keywords
	  `(
		(,dogescript-types-regexp     . font-lock-type-face)
		(,dogescript-constants-regexp . font-lock-constant-face)
		(,dogescript-functions-regexp . font-lock-function-name-face)
		(,dogescript-keywords-regexp  . font-lock-keyword-face)))
;; }}}

;; Command to comment/uncomment text
;; {{{
(defun dogescript-comment-dwim (arg)
  "Comment or uncomment current line or region in a smart way.
For detail, see `comment-dwim'."
  (interactive "*P")
  (require 'newcomment)
  (let (
		(comment-start "shh ") (comment-end "")
		)
	(comment-dwim arg)))
;; }}}

;; Syntax table
(defvar dogescript-syntax-table nil "Syntax table for `dogescript-mode'.")
(setq dogescript-syntax-table
	  (let ((synTable (make-syntax-table)))

		;; bash style comment: “# …”
		(modify-syntax-entry ?# "< b" synTable)
		(modify-syntax-entry ?\n "> b" synTable)

		synTable))

;; Defining the mode
;; It inherits from Javascript mode.
;; {{{
(define-derived-mode dogescript-mode javascript-mode
  "dogescript mode"
  "Major mode for editing Dogescript source code."

  ;; Code for syntax highlighting
  (setq font-lock-defaults '((dogescript-font-lock-keywords)))

  ;; Clearing memory
  ;; {{{
  (setq dogescript-keywords  nil)
  (setq dogescript-types     nil)
  (setq dogescript-constants nil)
  (setq dogescript-functions nil)
  ;; }}}

  (setq mode-name "dogescript")

  (define-key dogescript-mode-map [remap comment-dwim] 'dogescript-comment-dwim)
  )
;; }}}

(provide 'dogescript-mode)

