(setq doom-packages '(doom-themes treemacs))

;; For icons see https://github.com/domtronn/all-the-icons.el

(defun doom/init-doom-themes ()
  "Wait for doom to load to set up treemacs."
  (use-package doom-themes
    :defer t
    :init
    (spacemacs|use-package-add-hook treemacs
      :post-config
      (when (display-graphic-p)
        (add-hook 'treemacs-mode-hook 'doom/treemacs-mode)
        (doom-themes-treemacs-config)))))

(defun doom/post-init-treemacs ()
  "Do nothing. Work is done in init-doom-themes.")
