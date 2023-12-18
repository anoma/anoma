(in-package :resource-machine)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generalized helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (ty)
  `(or (cons ,ty (or null cons)) null))

(defmacro define-generic-print (type)
  `(defmethod print-object ((obj ,type) stream)
     (pprint-logical-block (stream nil)
       (print-unreadable-object (obj stream :type t)
         (format stream "~2I~{~_~A~^ ~}" (to-list obj))))))

(defgeneric to-list (class)
  (:documentation "Turns a class into a struct")
  (:method ((object standard-object))
    (mapcar (lambda (x) (slot-value object x))
            (mapcar #'c2mop:slot-definition-name
                    (c2mop:compute-slots (class-of object))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Logic Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> bool->int (boolean) (integer 0 1))
(defun bool->int (bool)
  (if bool 1 0))

(defun nullified? (x)
  (zerop x))

(defun created? (x)
  (not (nullified? x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal Interface helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; generate out destructure bind for function,

;; also generate out a `make-',function-name-resource for easy construction

;; (map 'list #'gensym (range 0 2))

(defun parse-predicate-arguments (argument-list)
  "parse the toplevel define-predicate arguments"
  (assert (<= 1 (length argument-list)))
  (let ((car (car argument-list)))
    (fill-arguments (and (listp car) (cdr car)) "PUBLIC" "PRIVATE")))

(-> fill-arguments ((or cons atom) string string) (cons symbol (cons symbol null)))
(defun fill-arguments (argument name1 name2)
  "Takes an argument which may be an atom or a list and returns a list"
  (let ((arg1 (gensym name1))
        (arg2 (gensym name2))
        (list-arg (if (listp argument) argument (list argument))))
    (destructuring-bind (&optional (a1 arg1) (a2 arg2)) list-arg
      (list a1 a2))))

(defun parse-inner (argument-list)
  "parse the inner arguments for a predicate declaration

Some good examples look like this

RESOURCE-MACHINE> (parse-inner '((mode pub priv)
                                 :created (c-res commitments)
                                 :nullified (n-res nullifiers)
                                 :data (nullifier-key resource-wanted amount)))
((MODE COMMITMENTS NULLIFIERS)
 ((NULLIFIER-KEY RESOURCE-WANTED AMOUNT) C-RES N-RES))

RESOURCE-MACHINE> (parse-inner '((mode pub priv)
                                 :created (c-res commitments)
                                 :nullified n-res))
((MODE COMMITMENTS #:NULLIFIERS607) (#:DATA608 C-RES N-RES))

RESOURCE-MACHINE> (parse-inner '(mode))
((MODE #:COMMITMENTS610 #:NULLIFIERS612)
 (#:DATA613 #:RESOURCE609 #:RESOURCE611))
"
  (assert (<= 1 (length argument-list)))
  (destructuring-bind (mode &key nullified created data) argument-list
    (mvlet ((c-res commitments
                   (values-list (fill-arguments created "RESOURCE" "COMMITMENTS")))
            (n-res nullifiers
                   (values-list (fill-arguments nullified "RESOURCE" "NULLIFIERS"))))
      (list (list (if (listp mode) (car mode) mode)
                  nullifiers
                  commitments)
            (list (or data (gensym "DATA")) n-res c-res)))))
