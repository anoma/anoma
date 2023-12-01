(in-package :resource-machine)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Predicate declarations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Predicate format
;; in real protocol the lists can be separated via 0 before consume,
;; and 1 before a created
;; for private data we know how many arguments we expect
;; public: (created-commitments created-nullifiers)
;; created-commitments: (list 𝔽ₒₘₘᵢₜₛ)
;; created-nullifiers: (list 𝔽ₙᵤₗᵢₑᵣₛ)
;; private: (private-inputs* created consumed)
;; private-inputs: 𝔽
;; created: (list resource)
;; consumed: (list resource)


;; example of define-predicate in action
(define-predicate test-example ((mode))
  (= 0 mode))

(define-predicate x-for-y ((mode pub priv)
                           :created (c-resources commitments)
                           :data (nullifier-key resource-wanted amount))
  ;; mode represents if it's created
  (and (or (created? mode)
           (<= amount
               (~>> (resources c-resources
                               :owned-by nullifier-key
                               :of (list resource-wanted))
                    (mapcar #'quantity)
                    sum)))
       ;; need to check label as well
       ;;
       ;; further I need to ensure there is only
       ;; 1 of this kind in a transaction, or else I can steal
       t))

(defparameter *special-label-x* 1)
(defparameter *special-label-y* 2)

;; unfinished
(define-predicate x-resource ((mode public private) :fixed-label *special-label-x*)
  t)

;; unfinished
(define-predicate y-resource ((mode public private) :fixed-label *special-label-y*)
  t)
