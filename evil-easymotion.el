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
    (require 'noflet)
    (require 'ace-jump-mode)
    (require 'evil)))


(defmacro evilem-generic (collector &rest follower)
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

(defmacro evilem-collect (func)
  "Repeatedly execute func, and collect the cursor positions into a list"
  `(noflet ((execute-motion ()
              (setq
                last-command ',func
                this-command ',func)
              (call-interactively ',func)))
     (let ((points ())
            (count 0)

            ;; make sure the motion doesn't move the window
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

(defmacro evilem-make-motion (func &optional pre-hook post-hook)
  "Automatically define an evil motion for func, naming it ace-func"
  `(evil-define-motion ,(make-symbol
                          (concat
                            "evilem-motion-"
                            (symbol-name func))) (count)
     (evil-without-repeat
       ,(when pre-hook
          `(call-interactively ,pre-hook))
       (let ((old-point (point)))
         (evil-enclose-ace-jump-for-motion
           (evilem-generic
             (evilem-collect ,func)
             ()))
         ;; handle the off-by-one case
         (when (< (point) old-point)
           (setq evil-this-type 'exclusive)))
       ,(when post-hook
          `(call-interactively ,post-hook)))))

(defmacro evilem-define (key motion &optional pre-hook post-hook)
  `(define-key evil-motion-state-map ,key
     (evilem-make-motion ,motion ,pre-hook ,post-hook)))

(define-key evil-motion-state-map (kbd "SPC") 'nil)

(defun default-keybindings (prefix)
  (noflet ((kbd-pfx (key)
             (kbd (concat prefix " " key))))
    (evilem-define (kbd-pfx "w") evil-forward-word-begin)
    (evilem-define (kbd-pfx "W") evil-forward-WORD-begin)
    (evilem-define (kbd-pfx "e") evil-forward-word-end)
    (evilem-define (kbd-pfx "E") evil-forward-WORD-end)
    (evilem-define (kbd-pfx "b") evil-backward-word-begin)
    (evilem-define (kbd-pfx "B") evil-backward-WORD-begin)
    (evilem-define (kbd-pfx "ge") evil-backward-word-end)
    (evilem-define (kbd-pfx "gE") evil-backward-WORD-end)

    (evilem-define (kbd-pfx "h") evil-backward-char)
    (evilem-define (kbd-pfx "j") next-line)
    (evilem-define (kbd-pfx "k") previous-line)
    (evilem-define (kbd-pfx "l") evil-forward-char)

    (evilem-define (kbd-pfx "g j") next-line)
    (evilem-define (kbd-pfx "g k") previous-line)
    (evilem-define (kbd-pfx "[[") evil-backward-section-begin)
    (evilem-define (kbd-pfx "[]") evil-backward-section-end)
    (evilem-define (kbd-pfx "]]") evil-forward-section-begin)
    (evilem-define (kbd-pfx "][") evil-forward-section-end)

    (evilem-define (kbd-pfx "(") evil-forward-sentence)
    (evilem-define (kbd-pfx ")") evil-backward-sentence)

    (evilem-define (kbd-pfx "n") evil-search-next)
    (evilem-define (kbd-pfx "N") evil-search-previous)

    (evilem-define (kbd-pfx "-") evil-previous-line-first-non-blank)
    (evilem-define (kbd-pfx "+") evil-next-line-first-non-blank)))

;;; evil-easymotion.el ends here
