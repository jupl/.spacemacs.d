(setq-default
 git-magit-status-fullscreen t
 magit-push-always-verify nil
 magit-visit-ref-behavior '(create-branch checkout-branch)
 projectile-use-git-grep t
 vc-follow-symlinks t
 version-control-diff-tool (cond ((display-graphic-p) 'diff-hl)
                                 ((eq system-type 'darwin) 'git-gutter)
                                 (t nil))
 version-control-diff-side 'left)

(defvar git-plus-repositories-path (if (eq system-type 'darwin)
                                       "~/Repositories"
                                     "~/repositories")
  "Default path where repositories are hosted.")
