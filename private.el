;; Write customizations to an unused file
(setq custom-file (concat myspacemacs-path "private/custom.el"))

;; Handle local code that is not part of the repository
(load (concat myspacemacs-path "private/local.el") 'noerror)
