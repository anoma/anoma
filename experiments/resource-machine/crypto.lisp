(in-package :resource-machine)

(-> nullifier-key (ed25519-private-key) integer)
(defun nullifier-key (key)
  (values (octets->uint (ed25519-key-x key) 32)))

(-> reconstruct-private (integer) ed25519-private-key)
(defun reconstruct-private (number)
  (values (make-private-key :ed25519 :x (int->octets number 32))))

(-> reconstruct-public (integer) ed25519-public-key)
(defun reconstruct-public (number)
  (values (make-public-key :ed25519 :y (int->octets number 32))))

(-> create-nullifier (resource ed25519-private-key) integer)
(defun create-nullifier (resource key)
  (values
   (octets->uint (ironclad:sign-message key (int64->octets (nullifier-hash resource)))
                 64)))

;; Question when do even have the info to call verify-nullifier?
(-> verify-nullifier (ed25519-public-key t  (simple-array (unsigned-byte 8) (64))) t)
(defun verify-nullifier (key message signature)
  (verify-signature key message signature))

;; (~> (~>> (make-instance 'resource)
;;          nullifier
;;          int64->octets
;;          (ironclad:sign-message *private*))
;;     (octets->uint 64)
;;     (int->octets 64))
