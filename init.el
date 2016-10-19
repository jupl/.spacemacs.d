;; Set up environment variables
(with-eval-after-load 'exec-path-from-shell
  (exec-path-from-shell-setenv "INSIDE_EMACS" emacs-version))

;; Configuration variables
(setq-default
 myspacemacs--gui (display-graphic-p)
 myspacemacs--osx (eq system-type 'darwin)
 myspacemacs--font-size 12
 myspacemacs--neotree-size 10
 myspacemacs--fixed-font "DejaVu Sans Mono"
 myspacemacs--variable-font "DejaVu Sans"
 myspacemacs--path (file-name-directory load-file-name)
 myspacemacs--max-column 79
 myspacemacs--powerline-scale 1.0
 myspacemacs--use-flowtype nil)

;; Set custom-file to a separate location
(setq-default custom-file (concat myspacemacs--path ".custom.el"))

;; Load additional libs
(load (concat myspacemacs--path "hooks.el"))
(load (concat myspacemacs--path "layers.el"))
(load (concat myspacemacs--path "local.el"))

;; Set up hooks
(add-hook 'myspacemacs--layers-hook 'myspacemacs//layers)
(add-hook 'myspacemacs--init-hook 'myspacemacs//init)
(add-hook 'myspacemacs--user-init-hook 'myspacemacs//user-init)
(add-hook 'myspacemacs--user-config-hook 'myspacemacs//user-config)
(add-hook 'myspacemacs--after-hook 'myspacemacs//after)

(defun myspacemacs//layers ()
  "Configure layers/packages for Spacemacs."
  (setq-default
   dotspacemacs-additional-packages '(darkokai-theme
                                      darktooth-theme
                                      doom-themes
                                      flycheck-flow)
   dotspacemacs-configuration-layers '(colors
                                       docker
                                       editorconfig
                                       emacs-lisp
                                       html
                                       javascript
                                       markdown
                                       org-plus
                                       osx
                                       react
                                       restclient
                                       shell
                                       shell-scripts
                                       syntax-checking
                                       vimscript
                                       yaml
                                       (auto-completion
                                        :variables
                                        auto-completion-return-key-behavior nil
                                        auto-completion-tab-key-behavior 'complete
                                        auto-completion-enable-help-tooltip t)
                                       (clojure
                                        :variables
                                        clojure-enable-fancify-symbols t)
                                       (git
                                        :variables
                                        git-magit-status-fullscreen t)
                                       (version-control
                                        :variables
                                        version-control-diff-tool 'diff-hl))
   dotspacemacs-delete-orphan-packages t
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-excluded-packages '(vi-tilde-fringe)))

(defun myspacemacs//init ()
  "Configure Spacemacs."
  (setq-default
   dotspacemacs-always-show-changelog nil
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-default-font `(,myspacemacs--fixed-font
                               :size ,myspacemacs--font-size
                               :powerline-scale ,myspacemacs--powerline-scale)
   dotspacemacs-editing-style 'vim
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-emacs-leader-key "C-l"
   dotspacemacs-highlight-delimiters 'current
   dotspacemacs-enable-lazy-installation nil
   dotspacemacs-leader-key "SPC"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-S-l"
   dotspacemacs-mode-line-unicode-symbols myspacemacs--gui
   dotspacemacs-persistent-server (and myspacemacs--gui myspacemacs--osx)
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 5)
                                bookmarks)
   dotspacemacs-themes '(spacemacs-dark spacemacs-light darkokai darktooth)))

(defun myspacemacs//user-init ()
  "Configure packages before they are loaded."
  (setq-default
   all-the-icons-scale-factor 1
   all-the-icons-default-adjust 0
   darkokai-mode-line-padding 1
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
   linum-relative-current-symbol ""
   magit-push-always-verify nil
   neo-theme (if myspacemacs--gui 'arrow 'ascii)
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
   diff-hl-side (if myspacemacs--gui 'left 'right)
   git-gutter-fr:side 'left-fringe
   git-gutter-fr+-side 'left-fringe
   grep-highlight-matches t
   fill-column myspacemacs--max-column
   neo-banner-message nil
   neo-smart-open t
   neo-vc-integration '(face)
   neo-show-hidden-files nil
   neo-mode-line-type 'none)

  ;; Override the default variable pitch font
  (set-face-attribute 'variable-pitch nil
                      :family myspacemacs--variable-font)

  ;; Use fixed-pitch font in certain text mode faces
  (set-face-attribute 'fixed-pitch nil
                      :family myspacemacs--fixed-font)
  (with-eval-after-load 'org
    (mapc 'add-fixed-pitch-to-face '(org-code
                                     org-block
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

  ;; Appearance settings
  (linum-relative-on)

  ;; Additional hooks
  (add-hook 'conf-mode-hook 'myspacemacs//prog-mode)
  (add-hook 'css-mode-hook 'myspacemacs//css-mode)
  (add-hook 'css-mode-hook 'myspacemacs//prog-mode)
  (add-hook 'js2-mode-hook 'myspacemacs//js-mode)
  (add-hook 'prog-mode-hook 'myspacemacs//prog-mode)
  (add-hook 'react-mode-hook 'myspacemacs//js-mode)
  (add-hook 'text-mode-hook 'myspacemacs//text-mode)
  (add-hook 'yaml-mode-hook 'myspacemacs//yaml-mode)

  ;; Additional patterns to match files to major modes
  (add-to-list 'auto-mode-alist '("\\.swig$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tag$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.zsh$" . sh-mode))
  (add-to-list 'auto-mode-alist '("\\.npmignore$" . gitignore-mode))
  (add-to-list 'auto-mode-alist '("/\\editorconfig.symlink$" . conf-unix-mode))
  (add-to-list 'auto-mode-alist '("/\\gitignore.symlink$" . gitignore-mode))
  (add-to-list 'auto-mode-alist '("/\\ideavimrc.symlink$" . vimrc-mode))
  (add-to-list 'auto-mode-alist '("/\\vimrc.symlink$" . vimrc-mode))
  (auto-mode-with-symlink "babelrc" 'json-mode)
  (auto-mode-with-symlink "bowerrc" 'json-mode)
  (auto-mode-with-symlink "eslintrc" 'json-mode)
  (auto-mode-with-symlink "jsbeautifyrc" 'json-mode)
  (auto-mode-with-symlink "jshintrc" 'json-mode)
  (auto-mode-with-symlink "tern-project" 'json-mode)

  ;; Additional keybinds
  (define-key evil-motion-state-map ";" 'evil-ex)
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-visual-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
  (define-key evil-visual-state-map (kbd "k") 'evil-previous-visual-line)
  (define-key global-map (kbd "C-h h") nil)
  (define-key global-map (kbd "C-q") nil)
  (evil-leader/set-key
    ";" 'evilnc-comment-or-uncomment-lines
    "ff" 'ido-find-file
    "fF" 'spacemacs/helm-find-files)

  ;; Turn off company mode for some text related editing
  (spacemacs|disable-company markdown-mode)

  ;; Add toggle to display time
  (spacemacs|add-toggle display-time
    :status display-time-mode
    :on (display-time-mode)
    :off (display-time-mode -1)
    :documentation "Display time in modeline"
    :evil-leader "tmT")

  ;; Add toggle for syntax highlighting
  (spacemacs|add-toggle syntax-highlighting
    :status font-lock-mode
    :on (font-lock-mode)
    :off (font-lock-mode -1)
    :documentation "Syntax highlighting"
    :evil-leader "tS")

  ;; Turn off minor mode lines
  (spacemacs/toggle-mode-line-minor-modes-off)

  ;; Add Flow support to Flycheck if enabled
  (when myspacemacs--use-flowtype
    (use-package flycheck-flow)
    (flycheck-add-next-checker 'javascript-eslint 'javascript-flow))

  ;; Terminal specifics
  (unless myspacemacs--gui
    (when (stringp linum-format)
      (setq linum-format (concat linum-format " ")))
    (with-eval-after-load 'linum-relative
      (setq linum-relative-format (concat linum-relative-format " "))))

  ;; Set up custom centered cursor mode
  (define-global-minor-mode myspacemacs-centered-cursor-mode
    centered-cursor-mode
    myspacemacs//centered-cursor-toggle)
  (myspacemacs-centered-cursor-mode t)

  ;; GUI specifics
  ;; For icons see https://github.com/domtronn/all-the-icons.el
  (when myspacemacs--gui
    (add-hook 'neotree-mode-hook 'myspacemacs//neotree-mode)
    (setq-default powerline-default-separator nil)
    (when (member "all-the-icons" (font-family-list))
      (use-package doom-neotree)))

  ;; Clear variable set earlier
  (setenv "INSIDE_EMACS" nil))

(defun myspacemacs//after ()
  "Final configurations after everything is loaded."
  nil)

(defun myspacemacs//centered-cursor-toggle ()
  "Enable centered cursor mode for certain major modes."
  (unless (member major-mode '(spacemacs-buffer-mode
                               eshell-mode
                               shell-mode
                               term-mode))
    (centered-cursor-mode t)))

(defun myspacemacs//css-mode ()
  "Configure css mode."
  (smartparens-mode t))

(defun myspacemacs//js-mode ()
  (prettify-symbols-mode t)
  (setq-local prettify-symbols-alist '(("..." . ?…)
                                       ("function" . ?ƒ)
                                       ("return" . ?▪)
                                       ("yield" . ?γ))))

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
  (if (member major-mode '(nxml-mode yaml-mode))
      (myspacemacs//prog-mode)
    (unless (eq major-mode 'org-mode)
      (variable-pitch-mode t))
    (visual-line-mode t)))

(defun myspacemacs//yaml-mode ()
  "Configure yaml mode."
  (diff-hl-mode t))

(defun add-fixed-pitch-to-face (face)
  "Enforce fixed pitch to a FACE."
  (let* ((old-inherit (face-attribute face :inherit))
         (list (if (listp old-inherit) old-inherit `(,old-inherit)))
         (new-inherit (if (member 'fixed-pitch list)
                          list
                        (cons 'fixed-pitch list))))
    (set-face-attribute face nil :inherit new-inherit)))

(defun auto-mode-with-symlink (file mode)
  "Associate FILE (and .symlink) with MODE to auto-mode-alist."
  (add-to-list 'auto-mode-alist `(,(concat "/\\." file "$") . ,mode))
  (add-to-list 'auto-mode-alist `(,(concat "/" file ".symlink$") . ,mode)))
