(defvar *CONSTANTS* '())
(defvar *VARIABLES* '(x y z))
(defvar *FUNCTIONS* '(+ - *))
(defvar *RELATIONS* '(= < > ))
(defvar *PRED* '(and or not -> <- <->))
(defvar *VARIABLE-COUNT* 0)

(defun new-variable ()
  "Creates a fresh variable which is used nowhere else"
  (incf *VARIABLE-COUNT*))

(defun constant-p (term)
  (and (atom term) (member term *CONSTANTS*)))

(defun variable-p (term)
  (and (atom term) (member term *VARIABLES*)))

(defun eq-var (a b)
  (eql a b))

(defun function-p (term)
  (and (listp term) (member (first term) *FUNCTIONS*)))

(defun relation-p (formula)
  (and (listp formula) (member (first formula) *RELATIONS*)))

(defun replace-term (term replacements)
  "Replaces variables in a term by replacement terms, 
   as described in the replacements association list"
  (cond 
    ((constant-p term) term)
    ((variable-p term) (or (cdr (assoc term replacements)) term))
    ((function-p term) 
      (list* (first term) (maplist #'(lambda (x) (replace-term (first x) replacements)) (rest term))))
    (t (assert nil))))

(defun replace-formula (formula(replace-formula '(forall x (and (= x y) (= x z))) '((x . r) (y . t))) replacements)
  "Replaces a variable in a formula by a replacement term"
  (cond
    ((relation-p formula)
      (list* (first formula) (maplist #'(lambda (x) (replace-term (first x) replacements)) (rest formula))))
    ((and (listp formula) (member (first formula) *PRED*))
      (list* (first formula) (maplist #'(lambda (x) (replace-formula (first x) replacements)) (rest formula))))
    ((and (listp formula) (member (first formula) '(forall exists)))
      (let ((v (new-variable)))
	(assert (= (length formula) 3))
	(list (first formula) v (replace-formula (third formula) (acons (second formula) v replacements)))))
    (t (assert nil))))
