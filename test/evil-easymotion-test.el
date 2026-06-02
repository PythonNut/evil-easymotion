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
