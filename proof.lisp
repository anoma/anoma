(in-package :resource-machine)

(defclass proof-output ()
  ((program     :accessor program     :initarg :program     :initform 0)
   (commitments :accessor commitments :initarg :commitments :initform 0)
   (nullifiers  :accessor nullifiers  :initarg :nullifiers  :initform 0)
   (inputs      :accessor inputs      :initarg :inputs      :initform 0)))

(define-generic-print proof-output)

;; garbage unfinished
(-> check-proof-correspondence ((list-of integer) (list-of integer)) boolean)
(defun check-proof-correspondence (initial new)
  (equalp initial new))

(-> compliance-proof ((list-of (list-of integer)) (list-of (list-of resource))) proof-output)
(defun compliance-proof (instance witness)
  (declare (ignore instance witness))
  (values
   (make-instance 'proof-output
                  :program (constantly 1))))
