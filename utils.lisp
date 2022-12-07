(defun read-file-as-lines (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil nil)
          while line
          collect line)))
