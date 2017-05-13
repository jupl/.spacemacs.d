;; Configuration variables
(setq-default
 myspacemacs--centered-cursor-ignore '(spacemacs-buffer-mode
                                       eshell-mode
                                       shell-mode
                                       term-mode)
 )

;; Set up hooks
(add-hook 'myspacemacs-layers-hook 'myspacemacs/layers)
(add-hook 'myspacemacs-init-hook 'myspacemacs/init)
(add-hook 'myspacemacs-user-init-hook 'myspacemacs/user-init)
(add-hook 'myspacemacs-user-config-hook 'myspacemacs/user-config)

(defun myspacemacs/layers ()
  "Configure layers/packages for Spacemacs."
  (setq-default
   dotspacemacs-additional-packages '(madhat2r-theme)
   dotspacemacs-configuration-layers
   '(clojure
     colors
     command-log
     doom
     editorconfig
     emacs-lisp
     git-plus
     helm
     html
     javascript-plus
     markdown-plus
     mouse
     olivetti
     osx
     restclient
     shell
     shell-scripts
     syntax-checking
     vim-empty-lines
     vimscript
     yaml
     (auto-completion
      :variables
      auto-completion-enable-help-tooltip t
      auto-completion-enable-snippets-in-popup t
      auto-completion-enable-sort-by-usage t
      auto-completion-return-key-behavior nil
      auto-completion-tab-key-behavior 'complete)
     (go
      :variables
      gofmt-command "goimports")
     (org-plus
      :variables
      org-plus-html-pygments-style "xcode"
      org-plus-pdf-no-tex t
      org-plus-use-pygments t)
     (spell-checking
      :variables
      spell-checking-enable-by-default nil))
   dotspacemacs-distribution 'spacemacs))

(defun myspacemacs/init ()
  "Configure Spacemacs."
  (setq-default
   dotspacemacs-default-font `(,myspacemacs-fixed-font
                               :size ,myspacemacs-font-size
                               :powerline-scale ,myspacemacs-powerline-scale)
   dotspacemacs-editing-style 'vim
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-enable-lazy-installation nil
   dotspacemacs-enable-paste-transient-state t
   dotspacemacs-ex-command-key ";"
   dotspacemacs-frame-title-format "Spacemacs"
   dotspacemacs-highlight-delimiters 'current
   dotspacemacs-leader-key "SPC"
   dotspacemacs-line-numbers '(:enabled-for-modes
                               conf-mode
                               conf-unix-mode
                               nxml-mode
                               prog-mode
                               yaml-mode
                               :relative t)
   dotspacemacs-mode-line-unicode-symbols (display-graphic-p)
   dotspacemacs-persistent-server (and (display-graphic-p)
                                       (eq system-type 'darwin))
   dotspacemacs-scratch-mode 'fundamental-mode
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 5)
                                bookmarks)
   dotspacemacs-themes '(doom-one spacemacs-light)
   dotspacemacs-visual-line-move-text t))

(defun myspacemacs/user-init ()
  "Configure packages before they are loaded."
  (setq-default
   create-lockfiles nil
   display-time-format "%a %m-%d %I:%M"
   display-time-default-load-average nil
   evil-echo-state nil
   evil-move-cursor-back nil
   exec-path-from-shell-check-startup-files nil
   helm-mode-handle-completion-in-region nil
   frame-resize-pixelwise t
   madhat2r-theme-org-height t
   whitespace-line-column myspacemacs-max-column)

  ;; Emacs 26
  (unless (fboundp 'display-buffer-in-major-side-window)
    (defalias
      'display-buffer-in-major-side-window
      'window--make-major-side-window)))

(defun myspacemacs/user-config ()
  "Configure packages after they are loaded."
  (setq-default
   frame-title-format '((:eval (myspacemacs//frame-title-format)))
   fill-column myspacemacs-max-column
   grep-highlight-matches t
   powerline-default-separator (if (display-graphic-p) 'bar nil))

  ;; Set font families
  (set-face-attribute 'variable-pitch nil :family myspacemacs-variable-font)
  (set-face-attribute 'fixed-pitch nil :family myspacemacs-fixed-font)

  ;; Set up eshell aliases
  ;; TODO Try to open buffers for emacs alias in a Spacemacs friendly way
  (with-eval-after-load 'em-alias
    (eshell/alias "emacs" "for i in ${eshell-flatten-list $*} {find-file $i}")
    (eshell/alias "erase-buffer" 'eshell/clear)
    (eshell/alias "hgrep" "history | grep $*")
    (eshell/alias "la" "ls -lAh $*"))

  ;; Additional hooks
  (add-hook 'conf-mode-hook 'myspacemacs//prog-mode)
  (add-hook 'css-mode-hook 'myspacemacs//css-mode)
  (add-hook 'css-mode-hook 'myspacemacs//prog-mode)
  (add-hook 'prog-mode-hook 'myspacemacs//prog-mode)
  (add-hook 'text-mode-hook 'myspacemacs//text-mode)
  (remove-hook 'prog-mode-hook 'linum-mode)
  (remove-hook 'text-mode-hook 'linum-mode)

  ;; Additional patterns to match files to major modes
  (add-to-list 'auto-mode-alist '("\\.zsh$" . sh-mode))
  (add-to-list 'auto-mode-alist '("\\.npmignore$" . gitignore-mode))
  (add-to-list 'auto-mode-alist '("/\\.?Brewfile$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("/\\.mbsyncrc$" . conf-mode))

  ;; Additional keybinds
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-visual-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
  (define-key evil-visual-state-map (kbd "k") 'evil-previous-visual-line)
  (define-key global-map (kbd "C-h h") nil)
  (define-key global-map (kbd "C-q") nil)
  (spacemacs/set-leader-keys
    ";" 'evilnc-comment-or-uncomment-lines
    "qd" nil
    "qD" nil
    "qr" nil
    "qR" nil)

  ;; Turn off minor mode lines
  (spacemacs/toggle-mode-line-minor-modes-off)

  ;; Turn on camel case motion
  (spacemacs/toggle-camel-case-motion-globally-on)

  ;; Add a space between line numbers and content in non-gui mode
  (unless (display-graphic-p)
    (when (stringp linum-format)
      (setq linum-format (concat linum-format " ")))
    (with-eval-after-load 'linum-relative
      (when (stringp linum-relative-format)
        (setq linum-relative-format (concat linum-relative-format " ")))))

  ;; Set up custom centered cursor mode
  (define-global-minor-mode myspacemacs-centered-cursor-mode
    centered-cursor-mode
    (lambda ()
      (unless (member major-mode myspacemacs--centered-cursor-ignore)
        (centered-cursor-mode t))))
  (myspacemacs-centered-cursor-mode t)

  ;; Clear variable set earlier
  (setenv "INSIDE_EMACS" nil))

(defun myspacemacs//frame-title-format ()
  "Custom frame title."
  (cond
   ((and buffer-file-truename (projectile-project-p))
    (concat "[" (projectile-project-name) "] " (file-relative-name
                                                buffer-file-truename
                                                (projectile-project-root))))
   ((projectile-project-p) (concat "[" (projectile-project-name) "]"))
   (buffer-file-truename buffer-file-truename)
   (t "Spacemacs")))

(defun myspacemacs//css-mode ()
  "Configure css mode."
  (smartparens-mode t))

(defun myspacemacs//prog-mode ()
  "Configure program mode."
  (spacemacs/toggle-truncate-lines-on)
  (rainbow-mode t))

(defun myspacemacs//text-mode ()
  "Configure text mode."
  (if (member major-mode '(conf-mode
                           conf-unix-mode
                           gitignore-mode
                           nxml-mode
                           yaml-mode))
      (myspacemacs//prog-mode)
      (spacemacs/toggle-truncate-lines-off)
      (toggle-word-wrap t)
      (variable-pitch-mode t)))
