(in-package :resource-machine)

;; Public interface

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prove and Verify functions
;; This is the external interface to the system
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set of commitments and nullifiers
;; custom witnesses, what logics, I.E. for fib the value we wish to compute really
;; hash(resource) --> commitments

;; resources themselves are the private input (witnesses)
;;   + we also send circuit specific data here as well (this will be the first)
;; commitment and nullifiers are the public inputs (instances)
;; the output should have commitments that we pass around publicly

(-> prove ((-> (list list) proof-output) list list) proof-output)
(defun prove (program instance witness)
  ;; create assertions on length for better error messages
  (values
   (funcall program instance witness)))

;; user creates: 1x --> 1y into the intent pool.
;;  - Special resource
;;    + set the ephermal flag to true
;;    + we don't need to check existence will consume it
;;    + it is not on the merkle tree (we can not create a valid merkle path)
;;    + User needs to create a proof of creating this intent (special resource)
;;       * don't need the resources the intent wants
;;       * input: resources that will be consumed (nullifiers)
;;       * output: resources that will be created (resource commits, added to merkle tree at end)
;;  - Can be created without enough resource
;;  - reality format: nock --> {special-resource, include_these_logics_into_transaction ....}
;; solver {1y --> 1x, 1x --> 1y}
;; solver will call prove
;;  - will consume the intent resource (and it's proof)
;;  - satisfy the intent


;; alice wants: x -> y, bob wants: y -> x
;; when x is consumed, create the ephemeral resource, that is the commitment that goes to x
;;   - solver is doing this
;;   - additional public input: nothing
;;   - additional private input: ephemeral resource (I can pass a kind) that confirms the counter increment
;; with the swap intent
;; total_number_of_swaps: +1
;; baked into the intent for x
;; resource_name: resorce_total_swaps(value: 10)
;; there must be another resource called increase_1
;; bond the two resource, every the swap intent is consumed

;; transaction is made that bob x, and alice y
;; resource_name: resorce_total_swaps(value: 11)

;; anchor check, check it existed in a previous computed at the end of
;; the last block. Or created in the partial transaction

;; 5 swaps
;; we have to get the correct values before hand


(-> verify (proof-output) (integer 0 1))
(defun verify (proof)
  (values (funcall (program proof) (inputs proof))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main Internal Interface abstractions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-predicate (function-name
                            (mode-pub-priv &key nullified created data (fixed-label 0))
                            &body body)
  "We define out a predicate, this constitutes a few things:

If the function is named f, then we define the following
f: this is the prove function for the given predicate
*f*: the hash of the function
make-resource-f: creating the resource
"
  ;; reconstruct the argument list for better parsing
  (let* ((unparsed (list mode-pub-priv :nullified nullified
                                       :created created
                                       :data data))
         (top-argument-names  (parse-predicate-arguments unparsed))
         (inner-argument-list (parse-inner unparsed))

         (hash-value (sxhash (list* unparsed body)))
         (make-fn    (intern (concat "MAKE-RESOURCE-" (symbol-name function-name))
                             (symbol-package function-name)))
         (data-list   (if (listp data) data (list data)))
         (result-name (gensym "RESULT"))

         ;; Ι know these by position in my inner-argument-list
         ;; basically 2nd and 3rd argument of the public arguments
         (commitments-variable (cadar inner-argument-list))
         (nullifier-variable   (caddar inner-argument-list))
         (public-arguments     (car top-argument-names)))
    `(prog1
         (defun ,function-name ,top-argument-names
           (destructuring-bind ,inner-argument-list
               (list ,@top-argument-names)
             (declare (ignorable ,@(alexandria:flatten inner-argument-list)))

             (let ((,result-name (progn ,@body)))
               (assert (typep ,result-name 'boolean))
               (make-instance
                'proof-output
                :commitments ,commitments-variable
                :nullifiers ,nullifier-variable
                :inputs ,public-arguments
                :program (lambda (public-again)
                           (bool->int
                            (and (check-proof-correspondence ,public-arguments
                                                             public-again)
                                 ,result-name)))))))

       (defparameter ,(intern (concat "*" (symbol-name function-name) "*")
                              (symbol-package function-name))
         ,hash-value)

       (defun ,make-fn (,@data-list &key
                                      (eph 0)   (nonce 0)    (npk 0)
                                      (rseed 0) (quantity 0) (label ,fixed-label))
         (make-instance 'resource :eph eph :nonce nonce :npk npk :rseed rseed
                                  :quantity quantity
                                  :label label
                                  :hash ,hash-value
                                  :data (list ,@data-list)))

       (setf (gethash ,hash-value *hash-functions*)
             (symbol-function ',function-name)))))


(-> create-intent (&key
                   (:commited  (list-of resource))
                   (:nullified (list-of nullifyable-resouce))
                   (:anchors   (list-of integer)))
    partial-transaction)
(defun create-intent (&key nullified commited anchors)
  "Creates an intent"
  (let* ((commitments  (mapcar #'commitment commited))
         (nullifiers   (mapcar (lambda (n)
                                 (create-nullifier (resource n) (npr-key n)))
                               nullified))
         (n-resources  (mapcar #'resource nullified))
         (public-data  (list nullifiers commitments))
         (private-data (list n-resources commited)))
    (flet ((create-proof (created-or-nullified resource)
             (prove (gethash (hash resource) *hash-functions*)
                    (cons created-or-nullified public-data)
                    (cons (data resource)      private-data))))
      (values
       (make-instance 'partial-transaction
                      :cms commitments
                      :nfs nullifiers
                      ;; For the transparent case we can just calculate this on the fly
                      :delta (append (mapcar (lambda (x) (list (kind x) (- (quantity x))))
                                             n-resources)
                                     (mapcar (lambda (x) (list (kind x) (quantity x)))
                                             commited))
                      :ans anchors
                      ;; this ensures that we have ran the VPS
                      :pr (cons (prove #'compliance-proof public-data private-data)
                                ;; 1 for created, 0 for consumed
                                (append (mapcar (lambda (x) (create-proof 0 x)) n-resources)
                                        (mapcar (lambda (x) (create-proof 1 x)) commited))))))))
