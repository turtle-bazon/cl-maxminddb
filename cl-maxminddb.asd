;;;; -*- mode: lisp -*-

(defsystem :cl-maxminddb
  :name "cl-maxminddb"
  :author "Azamat S. Kalimoulline <turtle@bazon.ru>"
  :licence "Lessor Lisp General Public License"
  :version "0.0.1.0"
  :description "CL MaxMind DB"
  :depends-on (babel
               cffi
               cffi-libffi
               ieee-floats
               iterate
               metabang-bind
               mmap)
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
