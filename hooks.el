(defvar myspacemacs-layers-hook nil "Hook for dotspacemacs/layers.")

(defvar myspacemacs-init-hook nil "Hook for dotspacemacs/init.")

(defvar myspacemacs-user-init-hook nil "Hook for dotspacemacs/user-init.")

(defvar myspacemacs-user-config-hook nil "Hook for dotspacemacs/user-config.")

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (run-hooks 'myspacemacs-layers-hook))

(defun dotspacemacs/init ()
  "Initialization function."
  (run-hooks 'myspacemacs-init-hook))

(defun dotspacemacs/user-init ()
  "Initialization function for user code."
  (run-hooks 'myspacemacs-user-init-hook))

(defun dotspacemacs/user-config ()
  "Configuration function for user code."
  (run-hooks 'myspacemacs-user-config-hook))
