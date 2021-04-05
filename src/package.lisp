;;;; -*- mode: lisp -*-

(defpackage #:ru.bazon.cl-maxminddb
  (:nicknames #:cl-maxminddb)
  (:use
   #:babel
   #:cffi
   #:cl
   #:ieee-floats
   #:iterate
   #:metabang-bind
   #:mmap)
  (:export
   #:with-mmdb
   #:make-mmdb
   #:mmdb-query
   #:get-in))
