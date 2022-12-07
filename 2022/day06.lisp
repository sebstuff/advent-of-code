(load "../utils.lisp")

(defun solve (filename num-chars-different)
  (let ((buffer (coerce (car (read-file-as-lines filename)) 'list)))
    (loop for i from 0 to (- (length buffer) num-chars-different)
          as start-marker = (subseq buffer i (+ i num-chars-different))
          when (= num-chars-different (length (remove-duplicates start-marker)))
            return (+ num-chars-different i))))

(defun part1 (filename)
  (solve filename 4))

(format t "part 1: ~a~%" (part1 "day06.input"))

(defun part2 (filename)
  (solve filename 14))

(format t "part 2: ~a~%" (part2 "day06.input"))
