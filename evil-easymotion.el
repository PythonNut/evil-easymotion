;;; evil-easymotion.el --- A port of vim's easymotion to emacs

;; Copyright (C) 2014 PythonNut

;; Author: PythonNut <pythonnut@pythonnut.com>
;; Keywords: convenience, evil, movement
;; Version: 20141120
;; URL: https://github.com/pythonnut/evil-easymotion.el
;; Package-Requires: ((emacs "24") (cl-lib "0.5") (ace-jump-mode "20140616.115") (noflet "20141102.654))

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
;; This is a clone of the popular easymotion package for vim, which
;; describes itself in these terms:

;; > EasyMotion provides a much simpler way to use some motions in vim.
;; > It takes the <number> out of <number>w or <number>f{char} by
;; > highlighting all possible choices and allowing you to press one key
;; > to jump directly to the target.

;; If you're having trouble picturing this, please visit the github repo
;; for a screencast.

;; Usage/status
;; ============

;; evil-easymotion, rather unsurprisingly can use evil. However, you don't
;; _need_ evil to use it. evil-easymotion can happily define motions for
;; regular emacs commands. With that said, evil is recommended, not
;; least because it's awesome.

;; Currently most motions are supported, and it's easy to define your own easymotions.

;;   (evilem-define (kbd "SPC w") evil-forward-word-begin)

;; To define easymotions for all motions that evil defines by default, add

;;   (evilem-default-keybindings "SPC")

;; This binds all motions under the prefix `SPC` in `evil-motion-state-map`. This is not done by default for motions defined manually. You will need to supply the prefix.

;; More advanced use-cases are detailed in the github README.

;;; Code:
(require 'cl-lib)
(eval-when-compile
  (progn
    (require 'noflet)
    (require 'ace-jump-mode)))


(defmacro evilem-generic (collector)
  "ace jump to candidates of collector using follower."
  (declare (indent 1))
  `(noflet ((ace-jump-search-candidate (str va-list)
              (mapcar (lambda (x)
                        (make-aj-position
                          :offset (1- x)
                          :visual-area (car va-list)))
                ,collector)))
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
                        (>= (point) win-start)
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
                  (ignore-errors (execute-motion))
                  t))
         (set-window-start (selected-window) win-start)
         (nreverse points)))))

(defmacro evilem-make-motion (name func &optional pre-hook post-hook vars)
  "Automatically define an evil motion for func, naming it ace-func"
  `(evil-define-motion ,name (count)
     (evil-without-repeat
       ,(when pre-hook
          `(funcall ,pre-hook))
       (let ,(append '((old-point (point))) vars)
         (evil-enclose-ace-jump-for-motion
           (evilem-generic (evilem-collect ,func)))
         ;; handle the off-by-one case
         (when (< (point) old-point)
           (setq evil-this-type 'exclusive)))
       ,(when post-hook
          `(funcall ,post-hook)))))

(defmacro evilem-make-motion-plain (name func &optional pre-hook post-hook vars)
  "Automatically define an evil motion for func, naming it ace-func"
  `(defun ,name ()
     (interactive)
     ,(when pre-hook
        `(funcall ,pre-hook))
     (let ,(append '((old-point (point))) vars)
       (evilem-generic (evilem-collect ,func)))
     ,(when post-hook
        `(funcall ,post-hook))))

(defmacro evilem-define (key motion &optional pre-hook post-hook vars)
  `(define-key evil-motion-state-map ,key
     (evilem-make-motion ,(make-symbol
                            (concat
                              "evilem-motion-"
                              (symbol-name motion)))
       ,motion ,pre-hook ,post-hook ,vars)))

(defmacro evilem-define-plain (key motion &optional pre-hook post-hook vars)
  `(global-set-key ,key
     (evilem-make-motion-plain ,(make-symbol
                                  (concat
                                    "evilem-motion-"
                                    (symbol-name motion)))
       ,motion ,pre-hook ,post-hook ,vars)))

(defun evilem-default-keybindings (prefix)
  (define-key evil-motion-state-map (kbd prefix) 'nil)
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

    (evilem-define (kbd-pfx "h") backward-char)
    (evilem-define (kbd-pfx "l") evil-forward-char)

    (evilem-define (kbd-pfx "j") next-line
      nil nil ((temporary-goal-column (current-column))
                (line-move-visual nil)))

    (evilem-define (kbd-pfx "k") previous-line
      nil nil ((temporary-goal-column (current-column))
                (line-move-visual nil)))

    (evilem-define (kbd-pfx "g j") next-line
      nil nil ((temporary-goal-column (current-column))
                (line-move-visual t)))

    (evilem-define (kbd-pfx "g k") previous-line
      nil nil ((temporary-goal-column (current-column))
                (line-move-visual t)))

    (evilem-define (kbd-pfx "t") evil-repeat-find-char
      (lambda ()
        (save-excursion
          (let ((evil-cross-lines t))
            (call-interactively 'evil-find-char-to))))
      nil
      ((evil-cross-lines t)))

    (evilem-define (kbd-pfx "T") evil-repeat-find-char
      (lambda ()
        (save-excursion
          (let ((evil-cross-lines t))
            (call-interactively 'evil-find-char-to-backward))))
      nil
      ((evil-cross-lines t)))

    (evilem-define (kbd-pfx "f") evil-repeat-find-char
      (lambda ()
        (save-excursion
          (let ((evil-cross-lines t))
            (call-interactively 'evil-find-char))))
      nil
      ((evil-cross-lines t)))

    (evilem-define (kbd-pfx "F") evil-repeat-find-char
      (lambda ()
        (save-excursion
          (let ((evil-cross-lines t))
            (call-interactively 'evil-find-char-backward))))
      nil
      ((evil-cross-lines t)))

    (evilem-define (kbd-pfx "[[") evil-backward-section-begin)
    (evilem-define (kbd-pfx "[]") evil-backward-section-end)
    (evilem-define (kbd-pfx "]]") evil-forward-section-begin)
    (evilem-define (kbd-pfx "][") evil-forward-section-end)

    (evilem-define (kbd-pfx "(") evil-forward-sentence)
    (evilem-define (kbd-pfx ")") evil-backward-sentence)

    (evilem-define (kbd-pfx "n") evil-search-next)
    (evilem-define (kbd-pfx "N") evil-search-previous)
    (evilem-define (kbd-pfx "*") evil-search-word-forward)
    (evilem-define (kbd-pfx "#") evil-search-word-backward)

    (evilem-define (kbd-pfx "-") evil-previous-line-first-non-blank)
    (evilem-define (kbd-pfx "+") evil-next-line-first-non-blank)))

(provide 'evil-easymotion)
;;; evil-easymotion.el ends here
