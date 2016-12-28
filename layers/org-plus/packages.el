(setq org-plus-packages '(org org-present))

;; For LaTeX and Beamer:
;; 1. Install LaTeX (and run "tlmgr update --self --all")
;; 2. Install Fira fonts
;; 3. Install Pygments
;; 4. Install the following TeX packages
;;   - beamertheme-metropolis
;;   - minted
;;   - wrapfig
;;   - capt-of
;;   - fvextra
;;   - ifplatform
;;   - xstring
;;   - pgfopts

(defun org-plus/post-init-org ()
  "Setup to customize org stuff."
  (spacemacs|disable-company org-mode)
  (spacemacs|use-package-add-hook org
    :post-config
    (progn
      (setq-default
       org-export-with-author nil
       org-export-with-section-numbers nil
       org-export-with-toc nil
       org-export-allow-bind-keywords t
       org-src-preserve-indentation t
       org-startup-folded 'showall)
      (add-to-list 'org-agenda-files "~/org")))
  (with-eval-after-load 'ox-html
    (setq-default
     org-html-head-include-scripts nil
     org-html-postamble nil
     org-html-style-include-default nil)
    (add-hook 'org-export-before-processing-hook 'org-plus/css-hook)
    (when org-plus-use-pygments
      (defun org-html-src-block (&rest arguments)
        (apply 'org-plus/org-html-src-block arguments))
      (setq-default org-html-htmlize-output-type nil)))
  (with-eval-after-load 'ox-latex
    (let ((options (append (cons "%latex" org-plus-pdf-options)
                           (if org-plus-use-pygments '("-shell-escape"))
                           '("-interaction nonstopmode"
                             "-output-directory %o %f"))))
      (setq-default
       org-latex-compiler "xelatex"
       org-latex-pdf-process `(,(string-join options " "))))
    (when org-plus-use-pygments
      (setq-default org-latex-listings 'minted)
      (add-to-list 'org-latex-packages-alist '("" "minted"))
      (add-to-list 'org-latex-logfiles-extensions "pyg"))
    (when org-plus-pdf-no-tex
      (add-to-list 'org-latex-logfiles-extensions "tex"))))

(defun org-plus/post-init-org-present ()
  "Setup to customize org-present."
  (add-hook 'org-present-mode-hook 'org-plus/org-present-setup)
  (add-hook 'org-present-mode-quit-hook 'org-plus/org-present-unsetup)
  (evilified-state-evilify nil org-present-mode-keymap
    (kbd "<prior>") 'org-present-prev
    (kbd "<next>") 'org-present-next
    "j" 'org-present-prev
    "k" 'org-present-next
    "." 'org-present-beginning))
