(defun myspacemacs/open-init ()
  "Open init configuration."
  (interactive)
  (find-file (concat myspacemacs-path "init.el")))

(defun myspacemacs/open-setup ()
  "Open user setup configuration."
  (interactive)
  (find-file (concat myspacemacs-path "setup.el")))

(defun myspacemacs/setup-keybinds ()
  "Add keybind to open additional files."
  (evil-leader/set-key
    "fed" 'myspacemacs/open-setup
    "fe C-d" 'myspacemacs/open-local))

(add-hook 'myspacemacs-user-config-hook 'myspacemacs/setup-keybinds)
