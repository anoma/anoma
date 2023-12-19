;; Break out into multiple packages at some point
(defpackage :resource-machine
  (:shadow :@)
  (:use :common-lisp :serapeum :ironclad :intbytes :trivia)
  (:export
;;;;;;;;
;;;;;; TODO make 3 packages, meta-level, zk-level and all to better
;;;;;;      show what level each of these components exist
;;;;;;;;


;;;;;;;;;;
;;;; Interface functions
;;;;;;;;;;

;;; From interface.lisp
   ;; these are the main interfaces functions of section 4
   ;; These exists at the `meta' level
   :prove
   :verify

   ;; These deal with defining predicates and intents
   ;; These exists at the `ZK-language' level
   :define-predicate
   :create-intent

;;; From resources.lisp
   ;; This is mainly a helper for dealing with filtering resources
   ;; These exists at the `ZK-language' level
   :resources

;;; From utility.lisp
   ;; These deal with minor helps for the zk logic
   ;; These exists at the `ZK-language' level
   :created?
   :nullified?
   :bool->int

;;;;;;;;;;
;;;; Generic accessors, types, and derived functions
;;;;;;;;;;

;;; From resource.lisp
   ;; Types
   ;; These exists at the `ZK-language' and the `meta' level
   :resource            :hash :label :quantity :data :eph :nonce :npk :rseed
   :resource-hash       :hcm :hnf :hkind :hΔ
   :nullifyable-resouce :resource :nullifier-private-key

   ;; Derived fields
   ;; These exists at the `ZK-language' and the `meta' level
   :commitment
   :address
   :nullifier-hash
   :kind
   :delta
   :note-cm
;;; From transaction.lisp

   ;; Types
   ;; These exists at the `ZK-language' and the `meta' level
   :partial-transaction :ans :cms :nfs :pr :delta :extra :pref))
