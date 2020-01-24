;;;; -*- mode: lisp -*-

(defsystem :cl-maxminddb
  :name "cl-maxminddb"
  :author "Azamat S. Kalimoulline <turtle@bazon.ru>"
  :licence "GNU Lesser General Public License, v3"
  :version "0.0.1.1"
  :description "CL MaxMind DB"
  :depends-on (#:babel
               #:cffi
               #:cffi-libffi
               #:ieee-floats
               #:iterate
               #:metabang-bind
               #:mmap)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "ip"
                  :depends-on
                  ("package"))
                 (:file "maxminddb"
                  :depends-on
                  ("package"
                   "ip"))))))
