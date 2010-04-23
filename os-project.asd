(defsystem :os-project
  :author ("Justin Caratzas" "Brit Butler")
  :version "0.1"
  :license "LLGPL"
  ;; sb-queue is being moved to sb-concurrency for sbcl 1.0.38
  ;; we'll need to search and replace...
  :depends-on (:split-sequence :sb-queue)
  :components ((:file "packages")
	       (:file "storage")
	       (:file "loader")
	       (:file "profiling")
	       (:file "scheduler")
	       (:file "cpu")
	       (:file "disassembler")
	       (:file "kernel")))
