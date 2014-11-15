
;; (eval-when-compile (require 'cl))

(eval-when-compile
  (progn
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
                        (< count (length ace-jump-mode-move-keys)))
                  (push (1+ (point)) points)
                  (setq count (1+ count))
                  (execute-motion)
                  t))
         (set-window-start (selected-window) win-start)
         (nreverse points)))))

(defmacro ace-motion (func)
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
  `(define-key evil-motion-state-map (kbd ,key) (ace-motion ,motion)))

(define-key evil-motion-state-map (kbd "SPC") 'nil)

(ease-motion "SPC w" evil-forward-word-begin)
(ease-motion "SPC W" evil-forward-WORD-begin)
(ease-motion "SPC e" evil-forward-word-end)
(ease-motion "SPC E" evil-forward-WORD-end)
(ease-motion "SPC b" evil-backward-word-begin)
(ease-motion "SPC B" evil-backward-WORD-begin)
(ease-motion "SPC ge" evil-backward-word-end)
(ease-motion "SPC gE" evil-backward-WORD-end)

(ease-motion "SPC h" evil-backward-char)
(ease-motion "SPC j" next-line)
(ease-motion "SPC k" previous-line)
(ease-motion "SPC l" evil-forward-char)

(ease-motion "SPC H" evil-backward-symbol)
(ease-motion "SPC L" evil-forward-symbol)

(ease-motion "SPC g j" next-line)
(ease-motion "SPC g k" previous-line)

(ease-motion "SPC s f" evil-forward-sexp)
(ease-motion "SPC s b" evil-backward-sexp)

(ease-motion "SPC [[" evil-backward-section-begin)
(ease-motion "SPC []" evil-backward-section-end)
(ease-motion "SPC ]]" evil-forward-section-begin)
(ease-motion "SPC ][" evil-forward-section-end)

(ease-motion "SPC L" evil-forward-symbol)
(ease-motion "SPC H" evil-backward-symbol)
(ease-motion "SPC (" evil-forward-sentence)
(ease-motion "SPC )" evil-backward-sentence)

(ease-motion "SPC n" evil-search-next)
(ease-motion "SPC N" evil-search-previous)

(ease-motion "SPC -" evil-previous-line-first-non-blank)
(ease-motion "SPC +" evil-next-line-first-non-blank)

(ease-motion "SPC M-h" evil-backward-sexp)
(ease-motion "SPC M-h" evil-backward-sexp)
(ease-motion "SPC M-j" evil-enter-sexp)
(ease-motion "SPC M-k" evil-backward-up-sexp)
(ease-motion "SPC M-l" evil-forward-sexp)

(ease-motion "SPC s f" evil-forward-sexp)
(ease-motion "SPC s b" evil-backward-sexp)
(ease-motion "SPC s d" evil-down-sexp)
(ease-motion "SPC s D" evil-backward-down-sexp)
(ease-motion "SPC s e" evil-up-sexp)
(ease-motion "SPC s U" evil-backward-up-sexp)
(ease-motion "SPC s n" evil-next-sexp)
(ease-motion "SPC s p" evil-previous-sexp)
