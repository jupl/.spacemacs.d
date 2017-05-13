(setq centered-cursor-packages '(centered-cursor))

(defun centered-cursor/post-init-centered-cursor ()
  "Use custom global centered cursor mode."
  (define-global-minor-mode centered-cursor/global-mode
    centered-cursor-mode
    centered-cursor/launch)
  (spacemacs|add-toggle centered-point-globally
    :mode centered-cursor/global-mode)
  (spacemacs/toggle-centered-point-globally-on))
