(setq org-extras-packages '(org org-present))

(defun org-extras/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config
    (progn
      (setq-default
       org-export-with-author nil
       org-export-with-section-numbers nil
       org-export-with-toc nil
       org-html-head-include-scripts nil
       org-html-postamble nil)
      (add-hook 'org-export-before-processing-hook
                'org-extras/inline-css-hook)))
  (spacemacs|use-package-add-hook org-present
    :post-init
    (add-hook 'org-present-mode-hook 'org-extras/org-present-setup)
    (add-hook 'org-present-mode-quit-hook 'org-extras/org-present-unsetup)))
