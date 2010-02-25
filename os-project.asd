(defsystem :os-project
  :author ("Justin Caratzas" "Brit Butler")
  :version "0.1"
  :license "LLGPL"
  :depends-on (:split-sequence)
  :components ((:file "packages")
	       (:file "storage")
	       (:file "loader")
	       (:file "cpu")
;	       (:file "schedulers")
	       ))
