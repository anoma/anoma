(in-package :resource-machine)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generating test resources
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; yes, it is in the transparent situation.
;; But in the shielded situation, the encryption of resources is
;; public. Users can fetch and decrypt.

;; public key is useful for concealing the private key when someone
;; sends you funds
(defun generate-resoruce-x ()
  "Randomly generate out a resource `*x*', making a fresh private key"
  (let ((private (generate-key-pair :ed25519)))
    (make-instance 'nullifyable-resouce
                   :resource (make-resource-x-resource
                              :quantity 1
                              :nonce 1
                              :npk (nullifier-key private))
                   :npr-key private)))

(defun generate-resoruce-y ()
  "Randomly generate out a resource `*y*', making a fresh private key"
  (let ((private (generate-key-pair :ed25519)))
    (make-instance 'nullifyable-resouce
                   :resource (make-resource-y-resource
                              :quantity 1
                              :nonce 1
                              :npk (nullifier-key private))
                   :npr-key private)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; resource-logic ---> x-for-y
;; an x is coming in a y is coming out

;; we have an x, we also have a y
;; we now call prove

;; we can potentially cut this, or keep this as resource x's keys
(defparameter *private* (generate-key-pair :ed25519))
(defparameter *public*
  (make-public-key :ed25519 :y (ed25519-public-key (ed25519-key-x *private*))))

;; swap x for y
(defun alice-intent ()
  (let* ((resource-x  (generate-resoruce-x))
         (private     (generate-key-pair :ed25519))
         (x-nullifier (npk (resource resource-x)))
         (ephemeral-resource
           (make-resource-x-for-y x-nullifier
                                  (kind (make-resource-y-resource))
                                  1
                                  ;; set real label later
                                  :label 1
                                  :quantity 1
                                  :eph 1
                                  :npk (nullifier-key private))))
    (list (create-intent :commited (list ephemeral-resource)
                         :nullified (list resource-x)
                         :anchors (list 0)) ; figure out what I ought to put here
          (make-instance 'nullifyable-resouce
                         :npr-key private
                         :resource ephemeral-resource)
          x-nullifier)))

(defun bob-intent ()
  (let* ((resource-y (generate-resoruce-y))
         (private    (generate-key-pair :ed25519))
         (y-nullifier (npk (resource resource-y)))
         (ephemeral-resource
           (make-resource-x-for-y y-nullifier
                                  (kind (make-resource-x-resource))
                                  1
                                  ;; set real label later
                                  :label 2
                                  :quantity 1
                                  :eph 1
                                  :npk (nullifier-key private))))
    (list (create-intent :commited (list ephemeral-resource)
                         :nullified (list resource-y)
                         :anchors (list 0)) ; figure out what I ought to put here
          (make-instance 'nullifyable-resouce
                         :npr-key private
                         :resource ephemeral-resource)
          y-nullifier)))

;; (mapcar (lambda (x) (funcall (program x) (inputs x)))
;;         (pr (car (alice-intent))))


(defun solve-bob-alice ()
  (mvlet* ((a-intent a-eph x-npk (values-list (alice-intent)))
           (b-intent b-eph y-npk (values-list (bob-intent)))
           ;; time to commit the new resources that help satisfy the
           ;; nullified resource
           (alice-y (make-resource-y-resource
                     :npk x-npk
                     :quantity 1))
           (bob-x (make-resource-x-resource
                   :npk y-npk
                   :quantity 1)))
    (combine-transactions
     (create-intent :commited (list alice-y bob-x)
                    :nullified (list b-eph a-eph)
                    ;; plz help here
                    :anchors nil)
     a-intent b-intent)))
