(setq-default
 git-magit-status-fullscreen t
 magit-commit-show-diff nil
 magit-visit-ref-behavior '(create-branch checkout-branch)
 projectile-use-git-grep t
 vc-follow-symlinks t
 version-control-diff-tool (cond ((display-graphic-p) 'diff-hl)
                                 ((eq system-type 'darwin) 'git-gutter)
                                 (t nil))
 version-control-diff-side 'left
 version-control-global-margin t)

(defvar git-plus-repositories-path "~/Repositories"
  "Default path where repositories are hosted.")
