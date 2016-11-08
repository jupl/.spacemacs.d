(defun org-plus/build-html-head ()
  "Inject css customizations to document."
  (let* ((inline (eq org-html-htmlize-output-type 'inline-css))
         (background-color (if inline (face-background 'default) 'unset))
         (color (if inline (face-foreground 'default) 'unset)))
    (format (org-plus/css-template)
            (org-plus/css-base-theme)
            background-color
            color)))

(defun org-plus/css-base-theme ()
  "Read base CSS theme from the filesystem."
  (with-temp-buffer
    (insert-file-contents (concat org-plus-path "theme.css"))
    (buffer-string)))

(defun org-plus/css-template ()
  "Read CSS template from the filesystem."
  (with-temp-buffer
    (insert-file-contents (concat org-plus-path "theme.html"))
    (buffer-string)))

(defun org-plus/inline-css-hook (exporter)
  "If EXPORTER is html, add html customizations."
  (when (eq exporter 'html)
    (setq-local org-html-head-extra (org-plus/build-html-head))))

(defun org-plus/org-present-setup ()
  "Customize ui when org-present is about to start."
  (spacemacs/toggle-mode-line-off)
  (spacemacs/toggle-highlight-current-line-globally-off)
  (message ""))

(defun org-plus/org-present-unsetup ()
  "Undo customizations when org-present is finished."
  (spacemacs/toggle-mode-line-on)
  (spacemacs/toggle-highlight-current-line-globally-on))
