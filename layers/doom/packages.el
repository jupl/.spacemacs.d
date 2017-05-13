(setq doom-packages '(doom-themes neotree))

;; For icons see https://github.com/domtronn/all-the-icons.el

(defun doom/init-doom-themes ()
  "Wiat for doom to load to set up neotree."
  (use-package doom-themes
    :defer t
    :init
    (spacemacs|use-package-add-hook neotree
      :pre-config
      (setq-default
       neo-banner-message nil
       neo-mode-line-type 'none
       neo-show-hidden-files nil)
      :post-config
      (when (display-graphic-p)
        (add-hook 'neotree-mode-hook 'doom/neotree-mode)
        (doom-themes-neotree-config)))))

(defun doom/post-init-neotree ()
  "Do nothing. Work is done in init-doom-themes.")
