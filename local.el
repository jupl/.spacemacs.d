(setq-default myspacemacs--local (concat myspacemacs--path ".local.el"))

(when (file-exists-p myspacemacs--local)
  (ignore-errors (load myspacemacs--local)))

(add-hook
 'myspacemacs--user-config-hook
 (lambda ()
   (evil-leader/set-key "fe\C-d" 'myspacemacs//find-dotfile-local)))

(defun myspacemacs//find-dotfile-local ()
  (interactive)
  (find-file myspacemacs--local))
