(defun doom/neotree-mode ()
  "Adjust Neotree height."
  (defface doom-neotree-face `((nil :height ,doom-neotree-size)) nil)
  (buffer-face-set 'doom-neotree-face))
