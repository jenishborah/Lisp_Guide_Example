(defstruct person
  name
  age
  occupation
  Address)

(defun create-person (name age occupation address)
  (make-person :name name :age age :occupation occupation :address address))

(defun display-person (person)
  (format t "Name: ~a~%" (person-name person))
  (format t "Age: ~a~%" (person-age person))
  (format t "Occupation: ~a~%" (person-occupation person))
  (format t "address: ~a~%" (person-address person)))

;; Create a person record
(defvar *person* (create-person "Jenish Borah" 23 "Student" "Jalukbari"))

;; Display the person record
(display-person *person*)
