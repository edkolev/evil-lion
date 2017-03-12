;;; evil-lion.el --- Evil align operator, port of vim-lion -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2017 edkolev

;; Author: edkolev <evgenysw@gmail.com>
;; URL: http://github.com/edkolev/evil-lion
;; Keywords: evil, plugin, align
;; Package-Requires: ((align.el ""))

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
(require 'align)

;;;###autoload
(defun evil-lion-install ()
  (define-key evil-normal-state-map (kbd "gl") 'evil-lion-left)
  (define-key evil-normal-state-map (kbd "gL") 'evil-lion-right)
  (define-key evil-visual-state-map (kbd "gl") 'evil-lion-left)
  (define-key evil-visual-state-map (kbd "gL") 'evil-lion-right))

(defun evil-lion-valid-char-p (char)
  (not (memq char '(?\C-\[ ?\C-?))))

;;;###autoload
(evil-define-operator evil-lion-left (beg end char)
  "Align the text in the given region using CHAR. Spaces are added to
the left of the found CHAR.

If CHAR is \"/\" the user is propted interactively for a regular
expression instead of a single character"
  :move-point nil
  (interactive "<r>c")
  (when (evil-lion-valid-char-p char)
    (let ((regex (evil-lion--maybe-read-regex char)))
      (evil-lion--align-region 'left beg end regex))))

;;;###autoload
(evil-define-operator evil-lion-right (beg end char)
  "Align the text in the given region using CHAR. Spaces are added to
the right of the found CHAR.

If CHAR is \"/\" the user is propted interactively for a regular
expression instead of a single character"
  :move-point nil
  (interactive "<r>c")
  (when (evil-lion-valid-char-p char)
    (let ((regex (evil-lion--maybe-read-regex char)))
      (evil-lion--align-region 'right beg end regex))))

(defun evil-lion--maybe-read-regex (char)
  (if (eq char ?/)             ;; TODO RET should plain call align
      (read-string "Regexp: ") ;; TODO default value shoud be ?/
    (regexp-quote (format  "%c" char))))

(defun evil-lion--align-region (type beg end regex)
  (when (> (length regex) 0)
    (let* ((indent-tabs-mode nil)
           (regexp
            (if (eq type 'left) (concat "\\(\\)" regex) (concat regex "\\(\\)")))
           (spacing 0)
           (repeat t)
           (group 1)
           (rule
            (list (list nil (cons 'regexp regexp)
                        (cons 'group group)
                        (cons 'spacing spacing)
                        (cons 'repeat repeat)))))
      (align-region beg end 'entire rule nil nil))))

(provide 'evil-lion)

;;; evil-lion.el ends here
