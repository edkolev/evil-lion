;;; evil-lion.el --- Evil align operator, port of vim-lion -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2017 edkolev

;; Author: edkolev <evgenysw@gmail.com>
;; URL: http://github.com/edkolev/evil-lion
;; Package-Requires: ((emacs "24") (evil "1.0.0"))
;; Version: 0.0.1
;; Keywords: emulations, evil, vim

;; This file is NOT part of GNU Emacs.

;;; License:
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Evil align operator, port of vim-lion by Tom McDonald (https://github.com/tommcdo/vim-lion)
;;
;; Usage:
;;
;; (evil-lion-install)
;;
;; The above call will install "gl" evil operator, which is used as:
;;   gl TEXT-OBJECT SEPARATOR
;; for example,
;;   gl ip =
;; will align the paragraph on = signs
;;
;;; Code:

(require 'evil)

(defgroup evil-lion nil
  "Align operator for Evil."
  :prefix "evil-lion"
  :group 'evil)

(defcustom evil-lion-left-align-key (kbd "g l")
  "Default binding for ‘evil-lion-left’.

Must be set before the minor mode is enabled."
  :type `,(if (get 'key-sequence 'widget-type)
              'key-sequence
            'sexp)
  :group 'evil-lion)

(defcustom evil-lion-right-align-key (kbd "g L")
  "Default binding for ‘evil-lion-right’.

Must be set before the minor mode is enabled."

  :type `,(if (get 'key-sequence 'widget-type)
              'key-sequence
            'sexp)
  :group 'evil-lion)

;;;###autoload(autoload 'evil-lion-left "evil-lion" nil t)
(evil-define-operator evil-lion-left (count beg end char)
  "Align the text in the given region using CHAR. Spaces are added to
the left of the found CHAR.

If CHAR is \"/\" the user is propted interactively for a regular
expression instead of a single character"
  :move-point nil
  :type line
  (interactive "<c><r>c")
  (evil-lion--align count beg end 'left char))

;;;###autoload(autoload 'evil-lion-right "evil-lion" nil t)
(evil-define-operator evil-lion-right (count beg end char)
  "Align the text in the given region using CHAR. Spaces are added to
the right of the found CHAR.

If CHAR is \"/\" the user is propted interactively for a regular
expression instead of a single character"
  :move-point nil
  :type line
  (interactive "<c><r>c")
  (evil-lion--align count beg end 'right char))

(defun evil-lion--align (count beg end type char)
  "Align the region b/w BEG and END.

TYPE can be either 'left or 'right.
CHAR is the character to align with."
  (cond ((eq char ?\r)
         (evil-lion--plain-align beg end))
        ((evil-lion--valid-char-p char)
         (let ((regex (evil-lion--maybe-read-regex char)))
           (evil-lion--align-region type count beg end regex)))))

(defun evil-lion--plain-align (beg end)
  "Aligh with rules defined by the major mode.

BEG and END specify the region."
  (let ((indent-tabs-mode nil))
    (align beg end)))

(defun evil-lion--valid-char-p (char)
  "Return nil if the CHAR is invalid align character, e.g. DEL."
  (not (memq char '(?\e ?\d ?\b)))) ;; ESC, DEL, BS

(defun evil-lion--maybe-read-regex (char)
  "If CHAR is \"/\", ask the user for a regex. Otherwise regexp-quote CHAR."
  (if (eq char ?/)
      (read-string "Pattern [/]: " nil nil "/")
    (regexp-quote (format  "%c" char))))

(declare-function align-region "align")
(defun evil-lion--align-region (type count beg end regex)
  "Build input for (align-region) and call it.

TYPE can be either 'left or 'right.
If COUNT is 1, the alignment will be performed on the first occurance
only.
BEG and END specify the retion to align.
REGEX is the regex to align by."
  (when (> (length regex) 0)
    (let* ((indent-tabs-mode nil)
           (regexp
            (if (eq type 'left) (concat "\\(\\)" regex) (concat regex "\\(\\)")))
           (spacing 0)
           (repeat
            (if (eq count 1) nil t))
           (group 1)
           (rule
            (list (list nil (cons 'regexp regexp)
                        (cons 'group group)
                        (cons 'spacing spacing)
                        (cons 'repeat repeat)))))
      ;; if align-region isn't loaded, require it
      (unless (fboundp 'align-region)
        (require 'align))
      (align-region beg end 'entire rule nil nil))))

(defun evil-lion--bind-keys (mode)
  "Bind keys for the given minor MODE."
  (when evil-lion-left-align-key
    (evil-define-minor-mode-key 'normal mode evil-lion-left-align-key 'evil-lion-left)
    (evil-define-minor-mode-key 'visual mode evil-lion-left-align-key 'evil-lion-left))
  (when evil-lion-right-align-key
    (evil-define-minor-mode-key 'normal mode evil-lion-right-align-key 'evil-lion-right)
    (evil-define-minor-mode-key 'visual mode evil-lion-right-align-key 'evil-lion-right)))

;;;###autoload
(define-minor-mode evil-lion-mode
  "evil-lion global mode, defines align operators 'gl' and 'gL'.

  Align with `gl MOTION CHAR` or right-align with `gL MOTION CHAR`.

  If CHAR is `/` you will be prompted for a regular expression instead
  of a plain character.

  If CHAR is `RET` alignment will be performed with align.el's rules
  specific for the current major mode."
  :global t
  (evil-lion--bind-keys 'evil-lion-mode))

(provide 'evil-lion)

;;; evil-lion.el ends here
