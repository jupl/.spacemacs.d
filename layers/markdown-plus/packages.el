(setq markdown-plus-packages '(markdown-mode))

(defun markdown-plus/post-init-markdown-mode ()
  "Add variable pitch support to certain markdown mode faces."
  (spacemacs|disable-company markdown-mode)
  (spacemacs|use-package-add-hook markdown-mode
    :post-config
    (mapc 'markdown-plus/add-fixed-pitch-to-face
          '(markdown-pre-face
            markdown-inline-code-face
            markdown-comment-face
            markdown-language-keyword-face))))
