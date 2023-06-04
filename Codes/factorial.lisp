(defun factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(defun calculate-factorial ()
  (format t "Enter a number: ")
  (let ((number (parse-integer (read-line))))
    (format t "Factorial of ~a is ~a.~%" number (factorial number))))

(calculate-factorial)
