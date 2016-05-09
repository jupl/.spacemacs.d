(setq-default myspacemacs--local (concat myspacemacs--path "local.el"))

(when (file-exists-p myspacemacs--local)
  (ignore-errors (load myspacemacs--local)))
