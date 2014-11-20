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


(defmacro ace-generic (collector &rest follower)
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

(defmacro ace-motion-collect (func)
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

(defmacro ace-motion (func)
  "Automatically define an evil motion for func, naming it ace-func"
  `(evil-define-motion ,(make-symbol (concat "ace-" (symbol-name func))) (count)
     (evil-without-repeat
       (let ((pnt (point))
              (buf (current-buffer)))
         (evil-enclose-ace-jump-for-motion
           (ace-generic
             (ace-motion-collect ,func)
             ()))
         (when (and (equal buf (current-buffer))
                 (< (point) pnt))
           (setq evil-this-type 'exclusive))))))

(defmacro ease-motion (key motion)
  `(define-key evil-motion-state-map ,key (ace-motion ,motion)))

(define-key evil-motion-state-map (kbd "SPC") 'nil)

(defun default-keybindings (prefix)
  (noflet ((kbd-pfx (key)
             (kbd (concat prefix " " key))))
    (ease-motion (kbd-pfx "w") evil-forward-word-begin)
    (ease-motion (kbd-pfx "W") evil-forward-WORD-begin)
    (ease-motion (kbd-pfx "e") evil-forward-word-end)
    (ease-motion (kbd-pfx "E") evil-forward-WORD-end)
    (ease-motion (kbd-pfx "b") evil-backward-word-begin)
    (ease-motion (kbd-pfx "B") evil-backward-WORD-begin)
    (ease-motion (kbd-pfx "ge") evil-backward-word-end)
    (ease-motion (kbd-pfx "gE") evil-backward-WORD-end)

    (ease-motion (kbd-pfx "h") evil-backward-char)
    (ease-motion (kbd-pfx "j") next-line)
    (ease-motion (kbd-pfx "k") previous-line)
    (ease-motion (kbd-pfx "l") evil-forward-char)

    (ease-motion (kbd-pfx "H") evil-backward-symbol)
    (ease-motion (kbd-pfx "L") evil-forward-symbol)

    (ease-motion (kbd-pfx "g j") next-line)
    (ease-motion (kbd-pfx "g k") previous-line)

    (ease-motion (kbd-pfx "s f") evil-forward-sexp)
    (ease-motion (kbd-pfx "s b") evil-backward-sexp)

    (ease-motion (kbd-pfx "[[") evil-backward-section-begin)
    (ease-motion (kbd-pfx "[]") evil-backward-section-end)
    (ease-motion (kbd-pfx "]]") evil-forward-section-begin)
    (ease-motion (kbd-pfx "][") evil-forward-section-end)

    (ease-motion (kbd-pfx "L") evil-forward-symbol)
    (ease-motion (kbd-pfx "H") evil-backward-symbol)
    (ease-motion (kbd-pfx "(") evil-forward-sentence)
    (ease-motion (kbd-pfx ")") evil-backward-sentence)

    (ease-motion (kbd-pfx "n") evil-search-next)
    (ease-motion (kbd-pfx "N") evil-search-previous)

    (ease-motion (kbd-pfx "-") evil-previous-line-first-non-blank)
    (ease-motion (kbd-pfx "+") evil-next-line-first-non-blank)

    (ease-motion (kbd-pfx "M-h") evil-backward-sexp)
    (ease-motion (kbd-pfx "M-h") evil-backward-sexp)
    (ease-motion (kbd-pfx "M-j") evil-enter-sexp)
    (ease-motion (kbd-pfx "M-k") evil-backward-up-sexp)
    (ease-motion (kbd-pfx "M-l") evil-forward-sexp)

    (ease-motion (kbd-pfx "s f") evil-forward-sexp)
    (ease-motion (kbd-pfx "s b") evil-backward-sexp)
    (ease-motion (kbd-pfx "s d") evil-down-sexp)
    (ease-motion (kbd-pfx "s D") evil-backward-down-sexp)
    (ease-motion (kbd-pfx "s e") evil-up-sexp)
    (ease-motion (kbd-pfx "s U") evil-backward-up-sexp)
    (ease-motion (kbd-pfx "s n") evil-next-sexp)
    (ease-motion (kbd-pfx "s p") evil-previous-sexp)))

;;; evil-easymotion.el ends here
