;; Set up hooks
(add-hook 'myspacemacs-layers-hook 'myspacemacs/layers)
(add-hook 'myspacemacs-init-hook 'myspacemacs/init)
(add-hook 'myspacemacs-user-init-hook 'myspacemacs/user-init)
(add-hook 'myspacemacs-user-config-hook 'myspacemacs/user-config)

(defun myspacemacs/layers ()
  "Configuration layers declaration."
  (setq-default
   dotspacemacs-configuration-layers
   '((auto-completion
      :variables
      auto-completion-enable-help-tooltip t
      auto-completion-enable-snippets-in-popup t
      auto-completion-enable-sort-by-usage t
      auto-completion-return-key-behavior nil
      auto-completion-tab-key-behavior 'complete)
     (centered-cursor
      :variables
      centered-cursor-major-mode-blacklist '(spacemacs-buffer-mode
                                             eshell-mode
                                             shell-mode
                                             term-mode))
     clojure
     colors
     command-log
     doom
     emacs-lisp
     git-plus
     (go
      :variables
      go-tab-width 2
      gofmt-command "goimports")
     graphviz
     helm
     html
     javascript-plus
     markdown-plus
     mouse
     (olivetti
      :variables
      olivetti-hide-mode-line t)
     (org-plus
      :variables
      org-plus-html-pygments-style "xcode"
      org-plus-pdf-no-tex t
      org-plus-use-pygments t
      org-enable-org-journal-support t
      org-journal-dir "~/org/journal"
      org-want-todo-bindings t)
     osx
     plantuml
     restclient
     (shell
      :variables
      shell-default-shell 'eshell)
     shell-scripts
     (spell-checking
      :variables
      spell-checking-enable-by-default nil)
     syntax-checking
     (theming
      :variables
      theming-modifications `((doom-one
                               (linum-relative-current-face :underline nil)
                               (modeline-highlight :inherit nil)
                               (powerline-active1 :background "#1c1f24"
                                                  :foreground "#bbc2cf")
                               (powerline-inactive2 :background "#202328")
                               (sp-show-pair-match-face :foreground "#86dc2f"))
                              (doom-one-light
                               (linum-relative-current-face :underline nil)
                               (modeline-highlight :inherit nil)
                               (powerline-active1 :background "#e7e7e7"
                                                  :foreground ,"#383a42")
                               (powerline-inactive2 :background "#dfdfdf")
                               (sp-show-pair-match-face :background nil))
                              (spacemacs-dark
                               (linum-relative-current-face :bold nil
                                                            :underline nil))
                              (spacemacs-light
                               (linum-relative-current-face :bold nil
                                                            :underline nil))))
     vim-empty-lines
     vimscript
     yaml)
   dotspacemacs-distribution 'spacemacs))

(defun myspacemacs/init ()
  "Initialization function."
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
   dotspacemacs-line-numbers (append '(:enabled-for-modes)
                                     myspacemacs-prog-based-modes
                                     '(:relative t))
   dotspacemacs-mode-line-unicode-symbols (display-graphic-p)
   dotspacemacs-persistent-server (display-graphic-p)
   dotspacemacs-scratch-mode 'fundamental-mode
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 5)
                                bookmarks)
   dotspacemacs-visual-line-move-text t
   dotspacemacs-themes (if (display-graphic-p)
                           '(doom-one doom-one-light)
                           '(spacemacs-dark spacemacs-light))))

(defun myspacemacs/user-init ()
  "Initialization function for user code."
  (setq-default
   create-lockfiles nil
   display-time-default-load-average nil
   display-time-format "%a %m-%d %I:%M"
   evil-echo-state nil
   evil-move-cursor-back nil
   frame-resize-pixelwise t
   helm-mode-handle-completion-in-region nil
   line-spacing 0
   whitespace-line-column myspacemacs-max-column)

  ;; Emacs 26
  (unless (fboundp 'display-buffer-in-major-side-window)
    (defalias
      'display-buffer-in-major-side-window
      'window--make-major-side-window))

  ;; Fake tmux being set so it is not run in shells
  (setenv "TMUX" "faux"))

(defun myspacemacs/user-config ()
  "Configuration function for user code."
  (setq-default
   frame-title-format '((:eval (myspacemacs/frame-title-format)))
   fill-column myspacemacs-max-column
   grep-highlight-matches t
   line-spacing 0
   powerline-default-separator (if (display-graphic-p) 'bar nil))

  ;; Set font families
  (set-face-attribute 'variable-pitch nil :family myspacemacs-variable-font)
  (set-face-attribute 'fixed-pitch nil :family myspacemacs-fixed-font)

  ;; Set up eshell aliases
  (with-eval-after-load 'em-alias
    (eshell/alias "hgrep" "history | grep $*")
    (eshell/alias "la" "ls -lAh $*"))

  ;; Additional hooks
  (add-hook 'conf-mode-hook 'myspacemacs/prog-mode)
  (add-hook 'css-mode-hook 'myspacemacs/css-mode)
  (add-hook 'css-mode-hook 'myspacemacs/prog-mode)
  (add-hook 'prog-mode-hook 'myspacemacs/prog-mode)
  (add-hook 'text-mode-hook 'myspacemacs/text-mode)

  ;; Additional patterns to match files to major modes
  (add-to-list 'auto-mode-alist '("\\.svg$" . image-mode-as-text))
  (add-to-list 'auto-mode-alist '("/\\.mbsyncrc$" . conf-mode))
  (add-to-list 'auto-mode-alist '("/\\.npmignore$" . gitignore-mode))
  (add-to-list 'auto-mode-alist '("/Brewfile$" . ruby-mode))

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
    "qD" nil)

  ;; Add a space between line numbers and content in non-gui mode
  (unless (display-graphic-p)
    (when (stringp linum-format)
      (setq linum-format (concat linum-format " ")))
    (with-eval-after-load 'linum-relative
      (when (stringp linum-relative-format)
        (setq linum-relative-format (concat linum-relative-format " ")))))

  ;; Make tweaks to spacemacs
  (global-evil-search-highlight-persist -1)
  (spaceline-toggle-buffer-position-off)
  (spaceline-toggle-buffer-size-off)
  (spaceline-toggle-hud-off)
  (spacemacs/toggle-camel-case-motion-globally-on)
  (spacemacs/toggle-mode-line-minor-modes-off))

(defun myspacemacs/css-mode ()
  "Configure css mode."
  (smartparens-mode t))

(defun myspacemacs/frame-title-format ()
  "Custom frame title."
  (cond
   ((and buffer-file-truename (projectile-project-p))
    (concat "[" (projectile-project-name) "] " (file-relative-name
                                                buffer-file-truename
                                                (projectile-project-root))))
   ((projectile-project-p) (concat "[" (projectile-project-name) "]"))
   (buffer-file-truename buffer-file-truename)
   (t "Spacemacs")))

(defun myspacemacs/prog-mode ()
  "Configure program mode."
  (spacemacs/toggle-truncate-lines-on)
  (rainbow-mode t))

(defun myspacemacs/text-mode ()
  "Configure text mode."
  (if (member major-mode myspacemacs-prog-based-modes)
      (myspacemacs/prog-mode)
    (spacemacs/toggle-truncate-lines-off)
    (toggle-word-wrap t)
    (variable-pitch-mode t)))
