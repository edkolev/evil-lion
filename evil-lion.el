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
;;; Code:

(require 'evil)

;;;###autoload
(defun evil-lion-install ()
  (define-key evil-normal-state-map (kbd "gl") 'evil-lion))

(defun evil-lion-valid-char-p (char)
  (not (memq char '(?\C-\[ ?\C-?))))

;;;###autoload
(evil-define-operator evil-lion (beg end char)
  "Align the text in the given region using CHAR.

If CHAR is \"/\" the user is propted interactively for a regular
expression instead of a single character"
  :move-point nil
  (interactive "<r>c")
  (when (evil-lion-valid-char-p char)
    (let ((regex (if (eq char ?/)
                     (read-string "Regexp: ")
                   (format  "%c" char))))
      (when (> (length regex) 0)
        (evil-lion--align-region beg end regex)))))

(defun evil-lion--align-region (beg end regex)
  (let* ((regexp (concat "\\(\\)" regex))
         (spacing 0)
         (repeat nil)
         (group 1)
         (rule
          (list (list nil (cons 'regexp regexp)
                      (cons 'group group)
                      (cons 'spacing spacing)
                      (cons 'repeat repeat)))))
    (align-region beg end 'entire rule nil nil)))

(provide 'evil-lion)

;;; evil-lion.el ends here
