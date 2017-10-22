(setq javascript-plus-packages '(json-mode flycheck))

(defun javascript-plus/post-init-json-mode ()
  "Treat additional files as json."
  (add-to-list 'auto-mode-alist '("/\\.amethyst$" . json-mode))
  (add-to-list 'auto-mode-alist '("/\\.babelrc$" . json-mode))
  (add-to-list 'auto-mode-alist '("/\\.bowerrc$" . json-mode))
  (add-to-list 'auto-mode-alist '("/\\.eslintrc$" . json-mode))
  (add-to-list 'auto-mode-alist '("/\\.jsbeautifyrc$" . json-mode))
  (add-to-list 'auto-mode-alist '("/\\.jshintrc$" . json-mode))
  (add-to-list 'auto-mode-alist '("/\\.tern-project$" . json-mode)))

(defun javascript-plus/post-init-flycheck ()
  "Disable TSLint by default."
  (setq-default
   flycheck-disabled-checkers (cons 'typescript-tslint
                                    flycheck-disabled-checkers)))
