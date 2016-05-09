(setq-default
 myspacemacs--layers-hook nil
 myspacemacs--init-hook nil
 myspacemacs--user-init-hook nil
 myspacemacs--user-config-hook nil
 myspacemacs--after-hook nil)

(defun dotspacemacs/layers () (run-hooks 'myspacemacs--layers-hook))
(defun dotspacemacs/init () (run-hooks 'myspacemacs--init-hook))
(defun dotspacemacs/user-init () (run-hooks 'myspacemacs--user-init-hook))
(defun dotspacemacs/user-config ()
  (run-hooks 'myspacemacs--user-config-hook)
  (run-with-timer 0 nil 'dotspacemacs/after))
(defun dotspacemacs/after () (run-hooks 'myspacemacs--after-hook))
