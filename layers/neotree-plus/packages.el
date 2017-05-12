(setq neotree-plus-packages '(doom-themes neotree))

;; For icons see https://github.com/domtronn/all-the-icons.el

(defun neotree-plus/init-doom-themes ()
  "Wiat for doom to load to set up neotree."
  (use-package doom-themes
    :defer t
    :init
    (spacemacs|use-package-add-hook neotree
      :pre-config
      (setq-default
       neo-banner-message nil
       neo-mode-line-type 'none)
      :post-config
      (when (display-graphic-p)
        (add-hook 'neotree-mode-hook 'neotree-plus/neotree-mode)
        (doom-themes-neotree-config)))))

(defun neotree-plus/post-init-neotree ()
  "Do nothing. Work is done in init-doom-themes.")
