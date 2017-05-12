(setq email-packages '(mu4e))

(defun email/post-init-mu4e ()
  "Additional setup for mu4e."
  (add-hook 'mu4e-compose-mode-hook 'org-mu4e-compose-org-mode))
