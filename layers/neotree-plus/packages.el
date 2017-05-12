(setq neotree-plus-packages '(doom-themes neotree))

;; For icons see https://github.com/domtronn/all-the-icons.el

(defun neotree-plus/init-doom-themes ()
  "Wiat for doom to load to set up neotree."
  (use-package doom-themes
    :defer t
    :init
    (when (and (display-graphic-p)
               (member "all-the-icons" (font-family-list)))
      (spacemacs|use-package-add-hook neotree
        :post-config
        (progn
          (add-hook 'neotree-mode-hook 'neotree-plus/neotree-mode)
          (doom-themes-neotree-config))))))

(defun neotree-plus/post-init-neotree ()
  "Do nothing. Work is done in init-doom-themes.")
