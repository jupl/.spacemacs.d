(setq git-plus-packages '(git-commit magit))

(defun git-plus/post-init-git-commit ()
  "Tweak git-commit-mode buffer."
  (add-hook 'git-commit-mode-hook 'git-plus/git-commit-mode))

(defun git-plus/post-init-magit ()
  "Add keybind to custom magit clone command."
  (spacemacs/set-leader-keys "gc" 'git-plus/magit-clone))
