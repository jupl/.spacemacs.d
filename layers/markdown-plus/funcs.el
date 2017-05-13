(defun markdown-plus/add-fixed-pitch-to-face (face)
  "Enforce fixed pitch to a FACE."
  (let* ((old-inherit (face-attribute face :inherit))
         (list (if (listp old-inherit) old-inherit `(,old-inherit)))
         (new-inherit (if (member 'fixed-pitch list)
                          list
                        (cons 'fixed-pitch list))))
    (set-face-attribute face nil :inherit new-inherit)))
