(defsystem #:infinite-buffers
  :description "Infinite Buffers"
  :author "David Clark <daveloper9000@gmail.com>"
  :license  "Apache 2"
  :version "0.0.1"
  :depends-on ("cffi" "alexandria" "babel")
  :serial t
  :components ((:file "src/package")
	       (:file "src/utils")
	       (:file "src/io")
	       (:file "src/buffers")
	       (:file "src/segment")
	       (:file "src/coding")))

(defsystem #:infinite-buffers-test
  :description "Infinite Buffers"
  :author "David Clark <daveloper9000@gmail.com>"
  :license  "Apache 2"
  :version "0.0.1"
  :depends-on ("infinite-buffers" "fiveam" "alexandria")
  :serial t

  :components ((:file "test/test-package")
	       (:file "test/test-coding")))

