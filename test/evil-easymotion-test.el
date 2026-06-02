;;; evil-easymotion-test.el --- Tests for evil-easymotion  -*- lexical-binding: t; -*-

(require 'ert)
(require 'evil-easymotion)

(defvar evilem-test--search-count nil)

(defun evilem-test--search-backward-a ()
  (interactive)
  (cl-incf evilem-test--search-count)
  (when (> evilem-test--search-count 4)
    (error "Search appears to be stuck in an invisible overlay"))
  (search-backward "a"))

(defun evilem-test--search-forward-a ()
  (interactive)
  (cl-incf evilem-test--search-count)
  (when (> evilem-test--search-count 4)
    (error "Search appears to be stuck in an invisible overlay"))
  (when (eq (char-after) ?a)
    (forward-char))
  (search-forward "a")
  (goto-char (match-beginning 0)))

(defun evilem-test--tree-contains-p (needle tree)
  (or (equal needle tree)
      (and (consp tree)
           (or (evilem-test--tree-contains-p needle (car tree))
               (evilem-test--tree-contains-p needle (cdr tree))))))

(ert-deftest evilem-make-motion-sets-inclusive-type-for-function-quoted-motion ()
  (should (evilem-test--tree-contains-p
           '(setq evil-this-type 'inclusive)
           (macroexpand-1
            '(evilem-make-motion evilem-test-motion
               #'evil-forward-word-end)))))

(ert-deftest evilem-make-motion-sets-exclusive-type-for-function-quoted-motion ()
  (should (evilem-test--tree-contains-p
           '(setq evil-this-type 'exclusive)
           (macroexpand-1
            '(evilem-make-motion evilem-test-motion
               #'evil-forward-word-begin)))))

(ert-deftest evilem-motion-forward-word-end-continues-in-operator-state ()
  (let ((evil-was-enabled evil-mode))
    (unwind-protect
        (progn
          (evil-mode 1)
          (save-window-excursion
            (with-temp-buffer
              (switch-to-buffer (current-buffer))
              (insert "this is a word")
              (goto-char (point-min))
              (let (points
                    (evil-state 'operator))
                (cl-letf (((symbol-function #'evilem--jump)
                           (lambda (collected-points)
                             (setq points collected-points))))
                  (evilem-motion-forward-word-end))
                (should (equal (mapcar #'car points) '(4 7 9 14)))))))
      (unless evil-was-enabled
        (evil-mode -1)))))

(defun evilem-test--line-action-result (action result-fn)
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (insert "alpha zero\nline two\n")
      (goto-char (point-min))
      (move-to-column 3)
      (let ((avy-ring (make-ring 20))
            (kill-ring nil)
            kill-ring-yank-pointer
            result)
        (cl-letf (((symbol-function #'avy--process)
                   (lambda (_points _style-fn)
                     (ring-insert avy-ring (cons (point) (selected-window)))
                     (funcall action
                              (save-excursion
                                (forward-line 1)
                                (move-to-column 3)
                                (point)))
                     (setq result (funcall result-fn)))))
          (evilem-motion-next-line))
        result))))

(ert-deftest evilem-line-motion-actions-use-whole-line ()
  (save-window-excursion
    (dolist (action '(avy-action-copy
                      avy-action-kill-move
                      avy-action-kill-stay
                      avy-action-teleport))
      (should (equal (evilem-test--line-action-result
                      action
                      (lambda ()
                        (substring-no-properties (current-kill 0 t))))
                     "line two\n")))
    (dolist (action '(avy-action-kill-move
                      avy-action-kill-stay))
      (should (equal (evilem-test--line-action-result
                      action
                      #'buffer-string)
                     "alpha zero\n")))
    (dolist (action '(avy-action-yank
                      avy-action-yank-line))
      (should (equal (evilem-test--line-action-result
                      action
                      #'buffer-string)
                     "alpline two\nha zero\nline two\n")))
    (should (equal (evilem-test--line-action-result
                    #'avy-action-teleport
                    #'buffer-string)
                   "alpline two\nha zero\n"))
    (should (equal (evilem-test--line-action-result
                    #'avy-action-mark
                    (lambda ()
                      (buffer-substring (mark) (point))))
                   "line two\n"))))

(ert-deftest evilem-collect-skips-invisible-overlays-backward ()
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (insert "a\nhidden a\nz")
      (let ((visible-a (point-min))
            (hidden-a (save-excursion
                        (goto-char (point-min))
                        (search-forward "hidden ")
                        (point))))
        (let ((overlay (make-overlay hidden-a (1+ hidden-a))))
          (overlay-put overlay 'invisible t)
          (goto-char (point-max))
          (let* ((evilem-test--search-count 0)
                 (points (evilem--collect #'evilem-test--search-backward-a
                                          nil nil nil nil #'identity)))
            (should (equal points (list (cons visible-a (selected-window)))))
            (should (= evilem-test--search-count 3))))))))

(ert-deftest evilem-collect-skips-invisible-overlays-forward ()
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (insert "z\nhidden a\na")
      (let ((hidden-a (save-excursion
                        (goto-char (point-min))
                        (search-forward "hidden ")
                        (point)))
            (visible-a (save-excursion
                         (goto-char (point-max))
                         (search-backward "a"))))
        (let ((overlay (make-overlay hidden-a (1+ hidden-a))))
          (overlay-put overlay 'invisible t)
          (goto-char (point-min))
          (let* ((evilem-test--search-count 0)
                 (points (evilem--collect #'evilem-test--search-forward-a
                                          nil nil nil nil #'identity)))
            (should (equal points (list (cons visible-a (selected-window)))))
            (should (= evilem-test--search-count 3))))))))

;;; evil-easymotion-test.el ends here
