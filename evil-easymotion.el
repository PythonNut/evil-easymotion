;;; evil-easymotion.el --- A port of vim's easymotion to evil

;; Copyright (C) 2014 PythonNut

;; Author: PythonNut <pythonnut@pythonnut.com>
;; Keywords: convenience, evil
;; Version: 20141120
;; URL: https://github.com/pythonnut/evil-easymotion.el
;; Package-Requires: ((emacs "24") (evil "20141119.205") (cl-lib "0.5") (ace-jump-mode "20140616.115"))

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:


;;; Code:
(require 'cl-lib)
(eval-when-compile
  (progn
    (require 'names)
    (require 'noflet)
    (require 'ace-jump-mode)
    (require 'evil)))


(defmacro evil-easymotion-generic (collector &rest follower)
  "ace jump to candidates of collector using follower."
  (declare (indent 1))
  `(noflet ((ace-jump-search-candidate (str va-list)
              (mapcar (lambda (x)
                        (make-aj-position
                          :offset (1- x)
                          :visual-area (car va-list)))
                ,collector)))
     (setq ace-jump-mode-end-hook
       (list (lambda ()
               (setq ace-jump-mode-end-hook)
               ,@follower)))
     (ace-jump-do "")))

(defmacro evil-easymotion-collect (func)
  "Repeatedly execute func, and collect the cursor positions into a list"
  `(noflet ((execute-motion ()
              (setq
                last-command ',func
                this-command ',func)
              (call-interactively ',func)))
     (let ((points ())
            (count 0)
            (smooth-scroll-margin 0)
            (scroll-margin 0)
            (win-start (window-start))
            (win-end (window-end)))
       (save-excursion
         (execute-motion)
         (while (when (and
                        (> (1+ (point)) win-start)
                        (< (1+ (point)) win-end)
                        ;; TODO: fix this
                        ;; unfortunately, ace-jump does not have
                        ;; a way to dictate where the letter folding
                        ;; occurs. Without this, it will occur at
                        ;; locations close to the cursor, which
                        ;; is annoying because those locations
                        ;; are the ones most often moved to.
                        (< count (length ace-jump-mode-move-keys)))
                  (push (1+ (point)) points)
                  (setq count (1+ count))
                  (execute-motion)
                  t))
         (set-window-start (selected-window) win-start)
         (nreverse points)))))

(defmacro evil-easymotion-make-motion (func)
  "Automatically define an evil motion for func, naming it ace-func"
  `(evil-define-motion ,(make-symbol (concat "ace-" (symbol-name func))) (count)
     (evil-without-repeat
       (let ((pnt (point))
              (buf (current-buffer)))
         (evil-enclose-ace-jump-for-motion
           (evil-easymotion-generic
             (evil-easymotion-collect ,func)
             ()))
         (when (and (equal buf (current-buffer))
                 (< (point) pnt))
           (setq evil-this-type 'exclusive))))))

(defmacro evil-easymotion-define (key motion)
  `(define-key evil-motion-state-map
     ,key (evil-easymotion-make-motion ,motion)))

(define-key evil-motion-state-map (kbd "SPC") 'nil)

(defun default-keybindings (prefix)
  (noflet ((kbd-pfx (key)
             (kbd (concat prefix " " key))))
    (evil-easymotion-define (kbd-pfx "w") evil-forward-word-begin)
    (evil-easymotion-define (kbd-pfx "W") evil-forward-WORD-begin)
    (evil-easymotion-define (kbd-pfx "e") evil-forward-word-end)
    (evil-easymotion-define (kbd-pfx "E") evil-forward-WORD-end)
    (evil-easymotion-define (kbd-pfx "b") evil-backward-word-begin)
    (evil-easymotion-define (kbd-pfx "B") evil-backward-WORD-begin)
    (evil-easymotion-define (kbd-pfx "ge") evil-backward-word-end)
    (evil-easymotion-define (kbd-pfx "gE") evil-backward-WORD-end)

    (evil-easymotion-define (kbd-pfx "h") evil-backward-char)
    (evil-easymotion-define (kbd-pfx "j") next-line)
    (evil-easymotion-define (kbd-pfx "k") previous-line)
    (evil-easymotion-define (kbd-pfx "l") evil-forward-char)

    (evil-easymotion-define (kbd-pfx "g j") next-line)
    (evil-easymotion-define (kbd-pfx "g k") previous-line)

    (evil-easymotion-define (kbd-pfx "[[") evil-backward-section-begin)
    (evil-easymotion-define (kbd-pfx "[]") evil-backward-section-end)
    (evil-easymotion-define (kbd-pfx "]]") evil-forward-section-begin)
    (evil-easymotion-define (kbd-pfx "][") evil-forward-section-end)

    (evil-easymotion-define (kbd-pfx "(") evil-forward-sentence)
    (evil-easymotion-define (kbd-pfx ")") evil-backward-sentence)

    (evil-easymotion-define (kbd-pfx "n") evil-search-next)
    (evil-easymotion-define (kbd-pfx "N") evil-search-previous)

    (evil-easymotion-define (kbd-pfx "-") evil-previous-line-first-non-blank)
    (evil-easymotion-define (kbd-pfx "+") evil-next-line-first-non-blank)))

;;; evil-easymotion.el ends here
