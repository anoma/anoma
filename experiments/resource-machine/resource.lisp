(in-package :resource-machine)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Type declarations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass resource ()
  (;; 𝔽ₗ
   (pred-hash      :accessor hash     :type integer       :initarg :hash :initform 0)
   ;; 𝔽ₗₐₑₗ
   (label          :accessor label    :type integer       :initarg :label :initform 0)
   ;; 𝔽Q
   (quantity       :accessor quantity :type integer       :initarg :quantity :initform 0)
   ;; {𝔽ᵥ}
   (data           :accessor data     :type (list-of integer) :initarg :data :initform nil)
   ;; 𝔽₂
   (ephemorability :accessor eph      :type (integer 0 1) :initarg :eph :initform 0)
   ;; 𝔽ₙₒₙₑ
   (nonce          :accessor nonce    :type integer       :initarg :nonce :initform 0)
   ;; 𝔽ₙₚₖ
   (nullifier-key  :accessor npk      :type integer       :initarg :npk :initform 0)
   ;; Fᵣₛₑₑ
   (random-seed    :accessor rseed    :type integer       :initarg :rseed :initform 0)))

;; A resource with a specified hash function
(defclass resource-hash (resource)
  ((hash-commitment :accessor hcm   :initform #'sxhash)
   (hash-nullifier  :accessor hnf   :initform #'sxhash)
   (hash-kind       :accessor hkind :initform #'sxhash)
   (hash-delta      :accessor hΔ    :initform #'sxhash)))

(defclass nullifyable-resouce ()
  ((resource :accessor resource
             :type resource
             :initform (error "provide resource")
             :initarg :resource)
   (nullifier-private-key :accessor npr-key
                          :type ed25519-private-key
                          :initarg :npr-key))
  (:documentation
   "A resource with the given private nullifier key to properly nullify it"))

(define-generic-print resource)
(define-generic-print nullifyable-resouce)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Computed hashed data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; in the null proof case we can just return the object itself, and
;; this would help the balance check
(defgeneric commitment (resource)
  (:documentation "A resource commitment key")
  (:method ((o standard-object))
    ;; We compute it by the whole object
    (sxhash (to-list o))))

(defgeneric address (resource)
  (:documentation "A resource address key")
  (:method ((o standard-object))
    (commitment o)))

(defgeneric nullifier-hash (resource)
  (:documentation "The hashed nullifier-hash message of a given resource"))

(defgeneric kind (resource)
  (:documentation "The kind of resource"))

;; binding signature
;; sum(qᵢ₁ * kindᵢ₁ + rᵢ₁ * base_point … qᵢₙ * kindᵢₙ + rᵢₙ * base_point)
;; - sum(qⱼ₁ * kindⱼ₁ + rⱼ₁ * base_point … qⱼₙ * kindⱼₙ + rⱼₙ * base_point)
;; = r * base_point
(defgeneric delta (resource)
  (:documentation "Total quantity of kinds. This is the binding signature implementation.
This should be an elliptic curve hash"))

(defgeneric note-cm (resource)
  (:documentation "Total quantity of kinds"))

;; Overload it so any class that subclasses this gets the right
;; to-list value of a resource
(defmethod to-list ((r resource))
  (mapcar (lambda (x) (slot-value r x))
            (mapcar #'c2mop:slot-definition-name
                    (c2mop:compute-slots (find-class-safe 'resource)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hashed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; || means cons
;; note_cm = poseidon_hash(l || label || v || npk || nonce || \psi || eph || q || rcm)
;; nf = poseidon_hash(nullifier_key || nonce || \psi || note_cm)
;; \psi and rcm are intermediate variables derived from rseed.
;; nullifier_key is the secret key of npk

(defun note-cm-data (r)
  (append (to-list r) (list (commitment r))))

(defun commitment-data (resource)
  (to-list resource))

;; specs says this should be the same as `commitment-data'
;; Xuyang would say otherwise
;; Make a request about this (forum post)
(defun nullifier-data (resource)
  (list (npk resource) (nonce resource) (rseed resource) (note-cm resource)))

(defun kind-data (resource)
  (list (hash resource) (label resource)))

(defun delta-data (resource)
  (list (kind resource) (quantity resource)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specific versions for resource
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Not needed but w/e
(defmethod commitment ((r resource))
  (sxhash (commitment-data r)))

(defmethod nullifier-hash ((r resource))
  (sxhash (nullifier-data r)))

(defmethod kind ((r resource))
  (sxhash (kind-data r)))

(defmethod delta ((r resource))
  (sxhash (delta-data r)))

(defmethod note-cm ((r resource))
  (sxhash (note-cm-data r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specific versions for resource-hash
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod commitment ((r resource-hash))
  (funcall (hcm r) (commitment-data r)))

(defmethod nullifier-hash ((r resource-hash))
  (funcall (hnf r) (nullifier-data r)))

(defmethod kind ((r resource-hash))
  (funcall (hkind r) (kind-data r)))

(defmethod delta ((r resource-hash))
  (funcall (hΔ r) (delta-data r)))

(defmethod note-cm ((r resource-hash))
  (funcall (hcm r) (note-cm r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Resource Logic Helpers
;; These greatly help using the resource machine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> resources ((list-of resource) &key (:of t) (:owned-by t)) (list-of resource))
(defun resources (resources &key of owned-by)
  "returns a resource list with the specified credentials

:of specifies the kinds that are accepted
:owned-by specifies who owns it"
  (remove-if (lambda (r)
               (or (and owned-by (not (= owned-by (npk r))))
                   (and of       (not (member (kind r) of :test #'=)))))
             resources))
