(asdf:defsystem :resource-machine
  :depends-on (:serapeum :ironclad :babel :cl-intbytes :fset)
  :components
  ((:file "package")
   (:file "utility" :depends-on (package))
   (:file "scaffolding" :depends-on (package utility))
   (:file "resource"    :depends-on (package utility))
   (:file "transaction" :depends-on (package utility))
   (:file "crypto"      :depends-on (package utility resource))
   (:file "proof"       :depends-on (package utility resource))
   (:file "interface"   :depends-on (resource crypto transaction proof scaffolding))
   (:file "predicates"  :depends-on (interface))
   (:file "example"     :depends-on (predicates interface))))
