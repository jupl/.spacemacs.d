(defun git-plus/git-commit-mode ()
  "Tweak git-commit buffer."
  (variable-pitch-mode -1))

(defun git-plus/magit-clone ()
  "Wrap magit-clone call to use a default directory."
  (interactive)
  (let ((default-directory (expand-file-name git-plus-repositories-path)))
    (call-interactively 'magit-clone)))
