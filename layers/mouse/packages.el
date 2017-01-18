(setq mouse-packages '(disable-mouse))

(defun mouse/init-disable-mouse ()
  "Initialize disable-mouse."
  (use-package disable-mouse
    :init (spacemacs|add-toggle disable-mouse
            :mode global-disable-mouse-mode
            :documentation "Disables mouse input."
            :evil-leader "tM")
    :diminish global-disable-mouse-mode))
