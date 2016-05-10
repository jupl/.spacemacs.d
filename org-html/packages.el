(setq org-html-packages '(org))

(defun org-html/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config
    (progn
      (setq-default
       org-html-head-include-scripts nil
       org-html-postamble nil)
      (add-hook 'org-export-before-processing-hook 'org-html/inline-css-hook))))
