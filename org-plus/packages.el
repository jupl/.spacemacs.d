(setq org-plus-packages '(org org-present))

(defun org-plus/pre-init-org ()
  "Setup to customize org stuff."
  (spacemacs|use-package-add-hook org
    :post-init
    (spacemacs|disable-company org-mode)
    :post-config
    (progn
      (setq-default
       org-export-with-author nil
       org-export-with-section-numbers nil
       org-export-with-toc nil
       org-export-allow-bind-keywords t
       org-html-head-include-scripts nil
       org-html-postamble nil
       org-html-style-include-default nil
       org-latex-compiler "xelatex"
       org-latex-listings 'minted
       org-src-preserve-indentation t
       org-startup-folded 'showall)
      (add-to-list 'org-agenda-files "~/org")
      (add-to-list 'org-latex-packages-alist '("" "minted"))
      (add-hook 'org-export-before-processing-hook
                'org-plus/inline-css-hook))))

(defun org-plus/pre-init-org-present ()
  "Setup to customize org-present."
  (spacemacs|use-package-add-hook org-present
    :post-init
    (progn
      (add-hook 'org-present-mode-hook 'org-plus/org-present-setup)
      (add-hook 'org-present-mode-quit-hook 'org-plus/org-present-unsetup)
      (evilified-state-evilify nil org-present-mode-keymap
        (kbd "<prior>") 'org-present-prev
        (kbd "<next>") 'org-present-next
        "j" 'org-present-prev
        "k" 'org-present-next
        "." 'org-present-beginning))))
