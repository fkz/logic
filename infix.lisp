(defvar *OPERATOR-LIST*
  '((^) (* /) (+ -) (= != > <) (not) (and or) (-> <->)))

(defvar *OPERATOR-TYPE* 
  '((not infix)))


(defun convert-to-praefix (formula)
  "Convert a formula into praefix notation, thereby (eventually) replacing formula"
  (loop for f on formula do
    (if (listp (first f))
      (rplaca f (convert-to-praefix (first f)))))
  (loop for operators in *OPERATOR-LIST* do
    (loop for f on formula do
      (loop
	(let ((operator (second f)))
	  (if (not (and (atom operator) (member operator operators))) (return))
	  (rplaca f (list operator (first f) (third f)))
	  (rplacd f (nthcdr 3 f))))))
  (assert (null (cdr formula)))
  (setf formula (car formula)))

(defun convert-to-infix (formula)
  "Convert to infix notation"
  
)

(defun read-formulas-convert ()
  (loop
    (print (convert-to-praefix (read)))))


(defstruct operator-list ()
  elements
  (arity 2)
  (associativity 'noassoc))

;(defvar *OPERATORS* "operators, that are in infix position, divided up by priority"
;  (list (make-operator-list ( 

;(defvar infix->praefix (formula)
;  "Converts an infix to a praefix formula"
  