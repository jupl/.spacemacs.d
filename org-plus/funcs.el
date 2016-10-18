(defun org-plus/build-html-head ()
  "Inject css customizations to document."
  (let* ((inline (eq org-html-htmlize-output-type 'inline-css))
         (background-color (if inline (face-background 'default) 'unset))
         (color (if inline (face-foreground 'default) 'unset)))
    (format "<style type=\"text/css\">
  html { font-family: sans-serif; }
  pre.src { background-color: %s; color: %s; }
  .figure img {
    display: block;
    max-width: 100%%;
    margin: 0 auto;
    box-shadow: 0 2px 8px rgba(0, 0, 0, 0.4);
  }
  @media screen { pre.src { overflow: auto; } }
  @media print { pre.src { word-wrap: break-word; }}
</style>
" background-color color)))

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
