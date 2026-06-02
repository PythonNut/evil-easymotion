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
