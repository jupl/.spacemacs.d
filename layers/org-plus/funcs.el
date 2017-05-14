(defun org-plus/build-html-head ()
  "Inject css customizations to document."
  (let* ((inline (and (not org-plus-use-pygments)
                      (eq org-html-htmlize-output-type 'inline-css)))
         (background-color (if inline (face-background 'default) 'unset))
         (color (if inline (face-foreground 'default) 'unset)))
    (format (org-plus/css-template)
            (org-plus/css-base-theme)
            background-color
            color
            (if org-plus-use-pygments
                (shell-command-to-string (format
                                          "pygmentize -S %s -f html -a \"%s\""
                                          org-plus-html-pygments-style
                                          ".highlight pre"))
              ""))))

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

(defun org-plus/css-hook (exporter)
  "If EXPORTER is html, add html customizations."
  (when (eq exporter 'html)
    (setq-local org-html-head-extra (org-plus/build-html-head))))

(defun org-plus/org-html-src-block (src-block _contents info)
  (if (org-export-read-attribute :attr_html src-block :textarea)
      (org-html--textarea-block src-block)
    (let ((lang (org-element-property :language src-block))
          (code (org-element-property :value src-block)))
      (with-temp-buffer
        (insert code)
        (shell-command-on-region
         (point-min) (point-max)
         (format "pygmentize -f html -g -l %s" (or lang "text"))
         (buffer-name) t)
        (buffer-string)))))

(defun org-plus/org-present-setup ()
  "Customize ui when org-present is about to start."
  (when (fboundp 'spacemacs/toggle-vi-tilde-fringe-off)
    (spacemacs/toggle-vi-tilde-fringe-off))
  (when (fboundp 'spacemacs/toggle-vim-empty-lines-mode-off)
    (spacemacs/toggle-vim-empty-lines-mode-off))
  (spacemacs/toggle-mode-line-off)
  (spacemacs/toggle-highlight-current-line-globally-off)
  (message ""))

(defun org-plus/org-present-unsetup ()
  "Undo customizations when org-present is finished."
  (when (fboundp 'spacemacs/toggle-vi-tilde-fringe-on)
    (spacemacs/toggle-vi-tilde-fringe-on))
  (when (fboundp 'spacemacs/toggle-vim-empty-lines-mode-on)
    (spacemacs/toggle-vim-empty-lines-mode-on))
  (spacemacs/toggle-mode-line-on)
  (spacemacs/toggle-highlight-current-line-globally-on))

(defun org-plus/add-fixed-pitch-to-face (face)
  "Enforce fixed pitch to a FACE."
  (let* ((old-inherit (face-attribute face :inherit))
         (list (if (listp old-inherit) old-inherit `(,old-inherit)))
         (new-inherit (if (member 'fixed-pitch list)
                          list
                        (cons 'fixed-pitch list))))
    (set-face-attribute face nil :inherit new-inherit)))
