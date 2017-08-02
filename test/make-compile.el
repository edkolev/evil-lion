(setq files '("evil-lion.el"))
(setq byte-compile--use-old-handlers nil)
(setq byte-compile-error-on-warn t)
(mapc #'byte-compile-file files)
