(setq jenkins-packages '(jenkins))

(defun jenkins/init-jenkins ()
  "Initialize Jenkins."
  (use-package jenkins
    :defer t
    :init
    (spacemacs/set-leader-keys "aJ" 'jenkins)
    :config
    (let ((common-keymap (copy-keymap evil-evilified-state-map)))
      (define-key common-keymap "b" nil)
      (define-key common-keymap "v" nil)
      (let ((keymap (copy-keymap common-keymap)))
        (define-key keymap "RET" nil)
        (evilified-state-evilify-map jenkins-mode-map
          :mode jenkins-mode
          :evilified-map keymap))
      (let ((keymap (copy-keymap common-keymap)))
        (define-key keymap "1" nil)
        (define-key keymap "g" nil)
        (define-key keymap "$" nil)
        (evilified-state-evilify-map jenkins-job-view-mode-map
          :mode jenkins-job-view-mode
          :evilified-map keymap)))))
