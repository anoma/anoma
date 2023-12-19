;; This file relates to meta information to make the system run
(in-package :resource-machine)

(defparameter *hash-functions*
  (let ((hash (make-hash-table)))
    hash))
