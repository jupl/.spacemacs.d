(defun ui-plus/launch-centered-cursor ()
  "Launch centered cursor except for certain major modes."
  (unless (member major-mode ui-plus-centered-cursor-blacklist)
    (centered-cursor-mode t)))

(defun ui-plus/treemacs-mode ()
  "Adjust treemacs height."
  (defface ui-plus-treemacs-face `((nil :height ,ui-plus-treemacs-size)) nil)
  (buffer-face-set 'ui-plus-treemacs-face))

(defun ui-plus/treemacs-toggle-recursive (&optional arg)
  "Force recursive node toggle."
  (interactive "P")
  (treemacs-toggle-node t))
