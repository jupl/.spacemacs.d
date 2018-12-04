(setq ui-plus-packages '(centered-cursor-mode
                         disable-mouse
                         doom-themes
                         solaire-mode
                         treemacs))

(defun ui-plus/init-disable-mouse ()
  "Initialize disable-mouse."
  (use-package disable-mouse
    :defer t
    :init (progn
            (spacemacs|add-toggle disable-mouse
              :mode global-disable-mouse-mode
              :documentation "Disables mouse input."
              :evil-leader "tM")
            (spacemacs/toggle-disable-mouse-on))
    :config (spacemacs|hide-lighter global-disable-mouse-mode)))

(defun ui-plus/init-doom-themes ()
  "Wait for doom to load to set up treemacs."
  (use-package doom-themes
    :defer t
    :init
    (spacemacs|use-package-add-hook treemacs
      :post-config
      (when (display-graphic-p)
        (add-hook 'treemacs-mode-hook 'ui-plus/treemacs-mode)
        (doom-themes-treemacs-config)))))

(defun ui-plus/init-solaire-mode ()
  "Initialize Solaire."
  (use-package solaire-mode
    :hook
    ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
    :config
    (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)
    (solaire-mode-swap-bg)))

(defun ui-plus/post-init-centered-cursor-mode ()
  "Use custom global centered cursor mode."
  (define-global-minor-mode ui-plus/centered-cursor-global-mode
    centered-cursor-mode
    ui-plus/launch-centered-cursor)
  (spacemacs|add-toggle centered-point-globally
    :mode ui-plus/centered-cursor-global-mode)
  (spacemacs/toggle-centered-point-globally-on))

(defun ui-plus/post-init-treemacs ()
  "Tweak treemacs."
  (setq-default
   treemacs-project-follow-cleanup t
   treemacs-show-hidden-files nil)
  (spacemacs|use-package-add-hook treemacs
    :post-config
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'treemacs-mode
        "<tab>" 'ui-plus/treemacs-toggle-recursive)
      (when (display-graphic-p)
        (add-hook 'treemacs-mode-hook 'ui-plus/treemacs-mode)))))
