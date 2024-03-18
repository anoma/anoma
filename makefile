default:

keygen:
	openssl req -x509 -newkey ec -pkeyopt ec_paramgen_curve:secp384r1 -days 3650 -nodes -keyout key.pem -out cert.pem -subj /CN=none

.PHONY: default keygen
