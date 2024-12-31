
(in-package :resource-machine)

;; Convert an ed25519 private key to an integer representation
(-> nullifier-key (ed25519-private-key) integer)
(defun nullifier-key (key)
  (values (octets->uint (ed25519-key-x key) 32)))

;; Reconstruct an ed25519 private key from an integer
(-> reconstruct-private (integer) ed25519-private-key)
(defun reconstruct-private (number)
  (values (make-private-key :ed25519 :x (int->octets number 32))))

;; Reconstruct an ed25519 public key from an integer
(-> reconstruct-public (integer) ed25519-public-key)
(defun reconstruct-public (number)
  (values (make-public-key :ed25519 :y (int->octets number 32))))

;; Create a nullifier for a given resource and private key
(-> create-nullifier (resource ed25519-private-key) integer)
(defun create-nullifier (resource key)
  (values
   (octets->uint (ironclad:sign-message key (int64->octets (nullifier-hash resource))) 64)))

;; Verify a nullifier using the public key, message, and signature
(-> verify-nullifier (ed25519-public-key t  (simple-array (unsigned-byte 8) (64))) t)
(defun verify-nullifier (key message signature)
  (values (ironclad:verify-signature key message signature)))

;; Sample usage (needs correction)
;; (~> (~>> (make-instance 'resource)
;;          nullifier
;;          int64->octets
;;          (ironclad:sign-message *private*))
;;     (octets->uint 64)
;;     (int->octets 64))
