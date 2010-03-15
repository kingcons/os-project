(defsystem :os-project
  :author ("Justin Caratzas" "Brit Butler")
  :version "0.1"
  :license "LLGPL"
  :depends-on (:split-sequence :cl-heap)
  :components ((:file "packages")
	       (:file "storage")
	       (:file "loader")
	       (:file "scheduler")
	       (:file "cpu")
	       (:file "disassembler")
	       ))
