(setq-default
 custom-file (concat myspacemacs--path "private/custom.el")
 myspacemacs--local (concat myspacemacs--path "private/local.el"))

(when (file-exists-p myspacemacs--local)
  (load myspacemacs--local 'noerror))

(add-hook
 'myspacemacs--user-config-hook
 (lambda ()
   (evil-leader/set-key "fe\C-d" 'myspacemacs//find-dotfile-local)))

(defun myspacemacs//find-dotfile-local ()
  "Open local configuration file."
  (interactive)
  (find-file myspacemacs--local))
