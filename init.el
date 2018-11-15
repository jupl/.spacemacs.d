(defvar myspacemacs-fixed-font
  (if (eq system-type 'darwin) "Fira Code" "Fira Mono")
  "Monospace font.")

(defvar myspacemacs-font-size nil
  "Base font size.")

(defvar myspacemacs-max-column 79
  "Maximum columns for each line.")

(defconst myspacemacs-path (file-name-directory load-file-name)
  "Base directory for .spacemacs.d path.")

(defvar myspacemacs-show-icons
  (and (display-graphic-p) (member "all-the-icons" (font-family-list)))
  "Whether icons can be shown. Use all-the-icons-install-fonts to install.")

(defvar myspacemacs-powerline-scale 1.4
  "Scale adjustment for mode line.")

(defvar myspacemacs-prog-based-modes '(conf-mode
                                       conf-unix-mode
                                       conf-space-mode
                                       dockerfile-mode
                                       editorconfig-conf-mode
                                       evil-tutor-mode
                                       json-mode
                                       gitconfig-mode
                                       gitignore-mode
                                       nxml-mode
                                       prog-mode
                                       react-mode
                                       snippet-mode
                                       typescript-mode
                                       web-mode
                                       yaml-mode)
  "List of prog based modes, including prog mode itself.")

(defvar myspacemacs-variable-font
  (if (fboundp 'mac-auto-operator-composition-mode)
      "Fira Sans"
    "DejaVu Sans")
  "Non-monospace font.")

;; Set server name to gui if GUI
(when (display-graphic-p)
  (setq server-name "gui"))

;; Enable ligatures if available
(when (fboundp 'mac-auto-operator-composition-mode)
  (mac-auto-operator-composition-mode t))

;; Add Homebrew packages to the load-path
(when (and (eq system-type 'darwin) (executable-find "keychain"))
  (seq-do
   (lambda (x) (apply #'setenv (split-string x "=")))
   (seq-filter
    (lambda (x) (string-prefix-p "SSH_" x))
    (split-string (shell-command-to-string "keychain --eval") "[;\n]+")))
  (let ((default-directory "/usr/local/share/emacs/site-lisp/"))
    (normal-top-level-add-subdirs-to-load-path)))

;; Load additional libs
(load (concat myspacemacs-path "hooks.el"))
(load (concat myspacemacs-path "keybinds.el"))
(load (concat myspacemacs-path "private.el"))
(load (concat myspacemacs-path "setup.el"))
