(defvar myspacemacs-fixed-font "Fira Code"
  "Monospace font.")

(defvar myspacemacs-font-size nil
  "Base font size.")

(defvar myspacemacs-max-column 79
  "Maximum columns for each line.")

(defvar myspacemacs-path (file-name-directory load-file-name)
  "Base directory for .spacemacs.d path.")

(defvar myspacemacs-powerline-scale 1.4
  "Scale adjustment for mode line.")

(defvar myspacemacs-variable-font
  (if (fboundp 'mac-auto-operator-composition-mode)
      "Fira Sans"
    "DejaVu Sans")
  "Nonmonospace font.")

;; Set up environment variables
(with-eval-after-load 'exec-path-from-shell
  (exec-path-from-shell-setenv "INSIDE_EMACS" emacs-version))

;; Enable ligatures if available
(when (fboundp 'mac-auto-operator-composition-mode)
  (mac-auto-operator-composition-mode t))

;; Add Homebrew packages to the load-path
(when (eq system-type 'darwin)
  (let ((default-directory "/usr/local/share/emacs/site-lisp/"))
    (normal-top-level-add-subdirs-to-load-path)))

;; Load additional libs
(load (concat myspacemacs-path "hooks.el"))
(load (concat myspacemacs-path "private.el"))
(load (concat myspacemacs-path "setup.el"))
