;; Set up environment variables
(with-eval-after-load 'exec-path-from-shell
  (exec-path-from-shell-setenv "INSIDE_EMACS" emacs-version))

;; Enable ligatures if available
(when (fboundp 'mac-auto-operator-composition-mode)
  (mac-auto-operator-composition-mode t))

;; Configuration variables
(setq-default
 myspacemacs--centered-cursor-ignore '(spacemacs-buffer-mode
                                       eshell-mode
                                       shell-mode
                                       term-mode)
 myspacemacs--fixed-font (if (fboundp 'mac-auto-operator-composition-mode)
                             "Fira Code"
                           "DejaVu Sans Mono")
 myspacemacs--font-size 12
 myspacemacs--gui (display-graphic-p)
 myspacemacs--max-column 79
 myspacemacs--neotree-size 10
 myspacemacs--macos (eq system-type 'darwin)
 myspacemacs--path (file-name-directory load-file-name)
 myspacemacs--powerline-scale 1.4
 myspacemacs--use-flowtype nil
 myspacemacs--variable-font (if (fboundp 'mac-auto-operator-composition-mode)
                                "Fira Sans"
                              "DejaVu Sans"))

;; Load additional libs
(load (concat myspacemacs--path "hooks.el"))
(load (concat myspacemacs--path "private.el"))

;; Set up hooks
(add-hook 'myspacemacs--layers-hook 'myspacemacs//layers)
(add-hook 'myspacemacs--init-hook 'myspacemacs//init)
(add-hook 'myspacemacs--user-init-hook 'myspacemacs//user-init)
(add-hook 'myspacemacs--user-config-hook 'myspacemacs//user-config)
(add-hook 'myspacemacs--after-hook 'myspacemacs//after)

(defun myspacemacs//layers ()
  "Configure layers/packages for Spacemacs."
  (setq-default
   dotspacemacs-additional-packages '(doom-themes)
   dotspacemacs-configuration-layers
   '(clojure
     colors
     command-log
     editorconfig
     emacs-lisp
     helm
     html
     javascript
     markdown
     mouse
     osx
     react
     restclient
     shell
     shell-scripts
     syntax-checking
     typescript
     vimscript
     yaml
     (auto-completion
      :variables
      auto-completion-enable-help-tooltip t
      auto-completion-enable-snippets-in-popup t
      auto-completion-enable-sort-by-usage t
      auto-completion-return-key-behavior nil
      auto-completion-tab-key-behavior 'complete)
     (git
      :variables
      git-magit-status-fullscreen t)
     (org-plus
      :variables
      org-plus-html-pygments-style "xcode"
      org-plus-pdf-no-tex t
      org-plus-use-pygments t)
     (spell-checking
      :variables
      spell-checking-enable-by-default nil)
     (version-control
      :variables
      version-control-diff-tool (cond (myspacemacs--gui 'diff-hl)
                                      (myspacemacs--macos 'git-gutter)
                                      (t nil))
      version-control-diff-side 'left))
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-excluded-packages '(vi-tilde-fringe)))

(defun myspacemacs//init ()
  "Configure Spacemacs."
  (setq-default
   dotspacemacs-default-font `(,myspacemacs--fixed-font
                               :size ,myspacemacs--font-size
                               :powerline-scale ,myspacemacs--powerline-scale)
   dotspacemacs-editing-style 'vim
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-enable-lazy-installation nil
   dotspacemacs-ex-command-key ";"
   dotspacemacs-highlight-delimiters 'current
   dotspacemacs-leader-key "SPC"
   dotspacemacs-line-numbers 'relative
   dotspacemacs-mode-line-unicode-symbols myspacemacs--gui
   dotspacemacs-persistent-server (and myspacemacs--gui myspacemacs--macos)
   dotspacemacs-scratch-mode 'fundamental-mode
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 5)
                                bookmarks)
   dotspacemacs-visual-line-move-text t))

(defun myspacemacs//user-init ()
  "Configure packages before they are loaded."
  (setq-default
   all-the-icons-scale-factor 1
   all-the-icons-default-adjust 0
   display-time-format "%a %m-%d %I:%M"
   display-time-default-load-average nil
   doom-enable-brighter-comments t
   doom-neotree-enable-file-icons t
   doom-neotree-enable-variable-pitch t
   evil-echo-state nil
   evil-move-cursor-back nil
   exec-path-from-shell-check-startup-files nil
   helm-mode-handle-completion-in-region nil
   js2-mode-show-parse-errors nil
   js2-mode-show-strict-warnings nil
   frame-title-format "%b"
   frame-resize-pixelwise t
   magit-push-always-verify nil
   magit-visit-ref-behavior '(create-branch checkout-branch)
   neo-theme (if myspacemacs--gui 'arrow 'ascii)
   projectile-use-git-grep t
   vc-follow-symlinks t
   whitespace-line-column myspacemacs--max-column)

  ;; Emacs 26
  (unless (fboundp 'display-buffer-in-major-side-window)
    (defalias
      'display-buffer-in-major-side-window
      'window--make-major-side-window)))

(defun myspacemacs//user-config ()
  "Configure packages after they are loaded."
  (setq-default
   grep-highlight-matches t
   fill-column myspacemacs--max-column
   neo-banner-message nil
   neo-mode-line-type 'none
   neo-smart-open t
   neo-vc-integration '(face)
   powerline-default-separator (if myspacemacs--gui 'arrow-fade nil))

  ;; Override the default variable pitch font
  (set-face-attribute 'variable-pitch nil :family myspacemacs--variable-font)

  ;; Use fixed-pitch font in certain text mode faces
  (set-face-attribute 'fixed-pitch nil :family myspacemacs--fixed-font)
  (with-eval-after-load 'org
    (mapc 'add-fixed-pitch-to-face '(org-code
                                     org-block
                                     org-formula
                                     org-table
                                     org-verbatim)))
  (with-eval-after-load 'markdown-mode
    (mapc 'add-fixed-pitch-to-face '(markdown-pre-face
                                     markdown-inline-code-face
                                     markdown-comment-face
                                     markdown-language-keyword-face)))

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
  (add-hook 'js2-mode-hook 'myspacemacs//js-mode)
  (add-hook 'prog-mode-hook 'myspacemacs//prog-mode)
  (add-hook 'react-mode-hook 'myspacemacs//js-mode)
  (add-hook 'text-mode-hook 'myspacemacs//text-mode)
  (remove-hook 'prog-mode-hook 'linum-mode)
  (remove-hook 'text-mode-hook 'linum-mode)

  ;; Additional patterns to match files to major modes
  (add-to-list 'auto-mode-alist '("\\.swig$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tag$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.zsh$" . sh-mode))
  (add-to-list 'auto-mode-alist '("\\.npmignore$" . gitignore-mode))
  (add-to-list 'auto-mode-alist '("/\\.amethyst$" . json-mode))
  (add-to-list 'auto-mode-alist '("/\\.babelrc$" . json-mode))
  (add-to-list 'auto-mode-alist '("/\\.bowerrc$" . json-mode))
  (add-to-list 'auto-mode-alist '("/\\.Brewfile$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("/\\.eslintrc$" . json-mode))
  (add-to-list 'auto-mode-alist '("/\\.jsbeautifyrc$" . json-mode))
  (add-to-list 'auto-mode-alist '("/\\.jshintrc$" . json-mode))
  (add-to-list 'auto-mode-alist '("/\\.tern-project$" . json-mode))
  (add-to-list 'auto-mode-alist '("/Brewfile$" . ruby-mode))

  ;; Additional keybinds
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-visual-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
  (define-key evil-visual-state-map (kbd "k") 'evil-previous-visual-line)
  (define-key global-map (kbd "C-h h") nil)
  (define-key global-map (kbd "C-q") nil)
  (evil-leader/set-key
    ";" 'evilnc-comment-or-uncomment-lines
    "qd" nil
    "qD" nil
    "qq" nil
    "qQ" nil
    "qr" nil
    "qR" nil)

  ;; Turn off company mode for some text related editing
  (spacemacs|disable-company markdown-mode)

  ;; Turn off minor mode lines
  (spacemacs/toggle-mode-line-minor-modes-off)

  ;; Turn on camel case motion
  (spacemacs/toggle-camel-case-motion-globally-on)

  ;; Turn off mouse
  (spacemacs/toggle-disable-mouse-on)

  ;; Add Flow support to Flycheck if enabled
  (when myspacemacs--use-flowtype
    (use-package flycheck-flow)
    (flycheck-add-next-checker 'javascript-eslint 'javascript-flow))

  ;; Add a space between line numbers and content in non-gui mode
  (unless myspacemacs--gui
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

  ;; GUI specifics
  ;; For icons see https://github.com/domtronn/all-the-icons.el
  (when myspacemacs--gui
    (add-hook 'neotree-mode-hook 'myspacemacs//neotree-mode)
    (when (member "all-the-icons" (font-family-list))
      (use-package doom-themes)
      (use-package doom-neotree)))

  ;; Clear variable set earlier
  (setenv "INSIDE_EMACS" nil))

(defun myspacemacs//after ()
  "Final configurations after everything is loaded."
  nil)

(defun myspacemacs//css-mode ()
  "Configure css mode."
  (smartparens-mode t))

(defun myspacemacs//js-mode ()
  "Configure js mode."
  (setq js--prettify-symbols-alist nil)
  (setq-local prettify-symbols-alist '(("function" . ?ƒ)
                                       ("return" . ?▪)
                                       ("yield" . ?γ)))
  (prettify-symbols-mode t))

(defun myspacemacs//neotree-mode ()
  "Apply consistent height to the Neotree buffer."
  (defface neotree-face `((nil :height ,(* myspacemacs--neotree-size 10))) nil)
  (buffer-face-set 'neotree-face))

(defun myspacemacs//prog-mode ()
  "Configure program mode."
  (spacemacs/toggle-truncate-lines-on)
  (spacemacs/toggle-line-numbers-on)
  (rainbow-mode t))

(defun myspacemacs//text-mode ()
  "Configure text mode."
  (if (member major-mode '(conf-mode conf-unix-mode nxml-mode yaml-mode))
      (myspacemacs//prog-mode)
    (spacemacs/toggle-truncate-lines-off)
    (toggle-word-wrap t)
    (variable-pitch-mode t)))

(defun add-fixed-pitch-to-face (face)
  "Enforce fixed pitch to a FACE."
  (let* ((old-inherit (face-attribute face :inherit))
         (list (if (listp old-inherit) old-inherit `(,old-inherit)))
         (new-inherit (if (member 'fixed-pitch list)
                          list
                        (cons 'fixed-pitch list))))
    (set-face-attribute face nil :inherit new-inherit)))
