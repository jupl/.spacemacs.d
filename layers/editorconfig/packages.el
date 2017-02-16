(setq editorconfig-packages '(editorconfig))

(defun editorconfig/init-editorconfig ()
  "Initialize EditorConfig."
  (use-package editorconfig
    :init (editorconfig-mode t)
    :config (spacemacs|hide-lighter editorconfig-mode)))
