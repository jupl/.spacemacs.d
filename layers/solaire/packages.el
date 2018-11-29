(setq solaire-packages '(solaire-mode))

(defun solaire/init-solaire-mode ()
  "Initialize Solaire."
  (use-package solaire-mode
    :hook
    ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
    :config
    (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)
    (solaire-mode-swap-bg)))
