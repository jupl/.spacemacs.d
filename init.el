(defvar myspacemacs-fixed-font "Fira Code"
  "Monospace font.")

(defvar myspacemacs-font-size nil
  "Base font size.")

(defvar myspacemacs-max-column 79
  "Maximum columns for each line.")

(defconst myspacemacs-path (file-name-directory load-file-name)
  "Base directory for .spacemacs.d path.")

(defvar myspacemacs-powerline-scale 1.4
  "Scale adjustment for mode line.")

(defvar myspacemacs-prog-based-modes '(conf-mode
                                       conf-unix-mode
                                       editorconfig-conf-mode
                                       evil-tutor-mode
                                       gitignore-mode
                                       nxml-mode
                                       prog-mode
                                       snippet-mode
                                       yaml-mode)
  "List of prog based modes, including prog mode itself.")

(defvar myspacemacs-variable-font
  (if (fboundp 'mac-auto-operator-composition-mode)
      "Fira Sans"
    "DejaVu Sans")
  "Non-monospace font.")

(defun myspacemacs/fix-linum-face ()
  "Fix face bleeding for linum faces and enforce consistency."
  (with-eval-after-load 'linum
    (set-face-attribute 'linum nil
                        :bold nil
                        :inverse-video nil
                        :underline nil))
  (with-eval-after-load 'linum-relative
    (set-face-attribute 'linum-relative-current-face nil
                        :bold t
                        :inverse-video nil
                        :underline nil)))

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

;; Fix linum face after theme changes
(add-hook 'spacemacs-post-theme-change-hook 'myspacemacs/fix-linum-face)

;; Load additional libs
(load (concat myspacemacs-path "hooks.el"))
(load (concat myspacemacs-path "keybinds.el"))
(load (concat myspacemacs-path "private.el"))
(load (concat myspacemacs-path "setup.el"))
