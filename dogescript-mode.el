;;; dogescript-mode.el --- Dogescript editing mode

;; Author:           Alexandre Dantas <eu@alexdantas.net>
;; URL:              https://github.com/alexdantas/dogescript-mode/
;; Version:          0.0.1
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
;; Load the file and lace the following on your emacs config file:

;;   (require 'dogescript-mode)

;; Notes:

;; wow
;;             such dogescript-mode
;;      very elisp
;;
;;                  much parenthesis
;;    wow
;;          stahp plz

;; Thanks a lot:
;; - http://emacs-fu.blogspot.com.br/2010/04/creating-custom-modes-easy-way-with.html
;; - http://www.emacswiki.org/cgi-bin/wiki/GenericMode
;; - http://ergoemacs.org/emacs/elisp_syntax_coloring.html

;;; Code:

(require 'generic-x)

;; First, let's define a hook so users can attach code
;; when this mode is launched
(defvar dogescript-mode-map
  (let ((dogescript-mode-map (make-keymap)))
	(define-key dogescript-mode-map "\C-j" 'newline-and-indent)
	dogescript-mode-map)

  "Keymap for DOGESCRIPT major mode")

(define-generic-mode 'dogescript-mode

  ;; Starting delimiter for comments
  '("shh ")

  ;; Some keywords
  '("very" "is" "such" "much" "wow" "wow&" "plz"
	"with" "rly" "but" "notrly" "many" "much"
	"so" "as" "dose")

  ;; More keywords
  ;; Note: First ones take precedence
  '(
	;; Strings (only type supported is '' - not "")
	("\'.*\'" . 'font-lock-string-face)

	;; Operators
	;; Note that they must be separate words
	;; (thus the \\b)
	;; {{{

	("\\bis\\b"   . 'font-lock-builtin-face)
	("\\band\\b"  . 'font-lock-builtin-face)
	("\\bor\\b"   . 'font-lock-builtin-face)
	("\\bnext\\b" . 'font-lock-builtin-face)

	;; Special negation operator
	("\\bnot\\b"  . 'font-lock-negation-char-face)

	;; }}}

	;; Built-ins
	("\\bmaybe\\b"   . 'font-lock-builtin)
	("\\btrained\\b" . 'font-lock-builtin)
	("\\bmaybe\\b"   . 'font-lock-builtin)

	;; Numbers are highlighted
	("\\b[0-9]+\\b" . 'font-lock-variable-name-face)

	;; Multi-line comments
	;; {{{

	;; Matching the comment boundaries (only full words)
	("\\bquiet\\b" . 'font-lock-comment-delimiter-face)
	("\\bloud\\b"  . 'font-lock-comment-delimiter-face)

	;; Matching everything between the comment boundaries
;	("\\bquiet\\b\\(.\\|\n\\)*\\bloud\\b" . 'font-lock-comment-face)

	;; }}}
	)

  ;; File extensions for this mode
  '("\\.djs$")

  ;; Other functions to call
  (list
   (lambda ()
	 ;; Forcing to insert spaces on this mode
	 ;; (since Dogescript doesn't recognize TABs)
	 (setq tab-width 4)
	 (setq indent-tabs-mode nil)

	 ;; Redundancy
	 (setq comment-start "shh ")
	 (setq comment-end   "")
	 ))

  ;; Doc string for this mode
  "Major mode for editing Dogescript source code"
  )

