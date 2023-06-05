(defstruct person
  name
  age
  occupation
  address)

(defun create-person-from-input ()
  (format t "Enter the name: ")
  (let ((name (read-line)))
    (format t "Enter the age: ")
    (let ((age (parse-integer (read-line))))
      (format t "Enter the occupation: ")
      (let ((occupation (read-line)))
        (format t "Enter the address: ")
        (let ((address (read-line)))
          (make-person :name name :age age :occupation occupation :address address))))))

(defun display-person (person)
  (format t "Name: ~a~%" (person-name person))
  (format t "Age: ~a~%" (person-age person))
  (format t "Occupation: ~a~%" (person-occupation person))
  (format t "Address: ~a~%" (person-address person)))

;; Create a person record from user input
(defvar *person* (create-person-from-input))

;; Display the person record
(display-person *person*)
