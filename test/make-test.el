
(let ((current-directory (file-name-directory load-file-name)))
  (setq evil-lion-test-path (expand-file-name "." current-directory))
  (setq evil-lion-root-path (expand-file-name ".." current-directory)))

(add-to-list 'load-path evil-lion-root-path)
(add-to-list 'load-path evil-lion-test-path)

(load (concat (file-name-as-directory evil-lion-test-path) "evil-lion-test.el") nil t)

(ert-run-tests-batch-and-exit)
