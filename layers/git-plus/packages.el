(setq git-plus-packages '(git-commit magit (magithub :excluded t)))

(defun git-plus/post-init-git-commit ()
  "Tweak git-commit-mode buffer."
  (add-hook 'git-commit-mode-hook 'git-plus/git-commit-mode))

(defun git-plus/post-init-magit ()
  "Add keybind to custom magit clone command."
  (spacemacs/set-leader-keys "gc" 'git-plus/magit-clone)
  (spacemacs|use-package-add-hook magit
    :post-config
    (magit-add-section-hook 'magit-status-sections-hook
                            'magit-insert-unpushed-to-upstream
                            'magit-insert-unpushed-to-upstream-or-recent
                            'replace)))
