(defvar myspacemacs-local (concat myspacemacs-path "private/local.el")
  "Path to local configuration.")

(defun myspacemacs/open-local ()
  "Open local configuration."
  (interactive)
  (find-file myspacemacs-local))

(defun myspacemacs/setup-open-local ()
  "Add keybind to open local configuration."
  (evil-leader/set-key "fe\C-d" 'myspacemacs/open-local))

;; Write customizations to an unused file
(setq custom-file (concat myspacemacs-path "private/custom.el"))

;; Handle local code that is not part of the repository
(load myspacemacs-local 'noerror)
(add-hook 'myspacemacs-user-config-hook 'myspacemacs/setup-open-local)
