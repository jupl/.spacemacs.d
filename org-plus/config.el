(defvar org-plus-path (file-name-directory load-file-name)
  "Base path for reading assets for this layer.")

(defvar org-plus-pdf-no-tex nil
  "If true, then do not leave TeX files when creating a PDF.")

(defvar org-plus-pdf-options nil
  "List of options to pass to command to generate PDF from LaTeX.")

(defvar org-plus-use-pygments nil
  "If true, then wire up for pygments.")
