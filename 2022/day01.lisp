(defun part1 (filename)
  (with-open-file (stream filename)
    (let ((current-elf 0)
          (max-elf 0))
      (loop for line = (read-line stream nil :eof)
            do (if (or (equal :eof line) (equal "" line))
                   ;; we're done with an elf, so need to process it
                   (progn
                     (when (> current-elf max-elf)
                       (setf max-elf current-elf))
                     (setf current-elf 0))
                   ;; still the same elf, just add to them
                   (incf current-elf (parse-integer line)))
            until (eq line :eof))
      max-elf)))

(assert (= 24000 (part1 "day01.example")))
(assert (= 69289 (part1 "day01.input")))


;;; returns the value and index of the minimum value in the array
(defun find-min (array)
  (let ((min-value nil)
        (min-index nil))
    (loop for value across array
          for index from 0
          do (when (or (= 0 index) (< value min-value))
               (setf min-value value)
               (setf min-index index)))
    (values min-value min-index)))

(defun part2 (filename)
  (with-open-file (stream filename)
    (let ((current-elf 0)
          (max-elves (make-array 3)))
      (loop for line = (read-line stream nil :eof)
            do (if (or (equal :eof line) (equal "" line))
                   ;; we're done with an elf, so need to process it
                   (multiple-value-bind (min-value min-index) (find-min max-elves)
                     (when (> current-elf min-value)
                       (setf (aref max-elves min-index) current-elf))
                     (setf current-elf 0))
                   ;; still the same elf, just add to them
                   (incf current-elf (parse-integer line)))
            until (eq line :eof))
      (apply '+ (coerce max-elves 'list)))))

(assert (= 45000 (part2 "day01.example")))
(assert (= 205615 (part2 "day01.input")))
