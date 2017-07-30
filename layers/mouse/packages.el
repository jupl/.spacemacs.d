(setq mouse-packages '(disable-mouse))

(defun mouse/init-disable-mouse ()
  "Initialize disable-mouse."
  (use-package disable-mouse
    :defer t
    :init (progn
            (spacemacs|add-toggle disable-mouse
              :mode global-disable-mouse-mode
              :documentation "Disables mouse input."
              :evil-leader "tM")
            (spacemacs/toggle-disable-mouse-on))
    :config (spacemacs|hide-lighter global-disable-mouse-mode)))
