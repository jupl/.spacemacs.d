(defun neotree-plus/neotree-mode ()
  "Adjust Neotree height."
  (defface neotree-plus-face `((nil :height ,neotree-plus-size)) nil)
  (buffer-face-set 'neotree-plus-face))
