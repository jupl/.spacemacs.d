(defun centered-cursor/launch ()
  "Launch centered cursor except for certain major modes."
  (unless (member major-mode centered-cursor-major-mode-blacklist)
    (centered-cursor-mode t)))
