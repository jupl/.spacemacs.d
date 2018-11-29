(defun doom/treemacs-mode ()
  "Adjust Neotree height."
  (defface doom-treemacs-face `((nil :height ,doom-treemacs-size)) nil)
  (buffer-face-set 'doom-treemacs-face))
