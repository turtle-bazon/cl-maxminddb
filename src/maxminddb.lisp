;;;; -*- mode: lisp -*-

(in-package #:ru.bazon.cl-maxminddb)

(defparameter *metadata-marker*
  (concatenate 'vector
               #(#xab #xcd #xef)
               (map 'vector #'char-code "MaxMind.com")))

(defstruct maxmind-database-metadata
  (node-count 0 :read-only t :type fixnum)
  (record-size 0 :read-only t :type fixnum)
  (ip-version 0 :read-only t :type fixnum)
  (database-type nil :read-only t :type string)
  (languages nil :read-only t :type vector)
  (binary-format-major-version 0 :read-only t :type fixnum)
  (binary-format-minor-version 0 :read-only t :type fixnum)
  (build-epoch 0 :read-only t :type fixnum)
  (description nil :read-only t :type list)
  (search-tree-size 0 :read-only t :type fixnum))

(defstruct maxmind-database
  (ptr nil :read-only t)
  (filename nil :read-only t)
  (size 0 :read-only t)
  (metadata nil))

(defun mread-raw-uchar (mmdb offset)
  (bind (((:structure maxmind-database- ptr) mmdb))
    (mem-ref ptr :uchar offset)))

(defun mread-raw-unsigned (mmdb offset length)
  (bind (((:structure maxmind-database- ptr) mmdb))
    (iter (for i from offset below (+ offset length))
          (for byte-index from  (- length 1) downto 0)
          (for byte = (mem-ref ptr :uchar i))
          (sum (ash byte (* 8 byte-index))))))

(defun mread-unsigned (mmdb offset length)
  (values (mread-raw-unsigned mmdb offset length)
          (+ offset length)))

(defun mread-special-pointer-length (mmdb offset length0)
  (declare (ignore mmdb))
  (bind ((size (ash length0 -3)))
    (ecase size
      ((0) (list 2 offset))
      ((1) (list 3 offset))
      ((2) (list 4 offset))
      ((3) (list 5 offset)))))

(defun mread-special-generic-length (mmdb offset length0)
  (bind ((offset (+ offset 1)))
    (ecase length0
      ((29) (list (+ 29 (mread-raw-uchar mmdb offset)) (+ offset 1)))
      ((30) (list (+ 285 (mread-raw-unsigned mmdb offset 2)) (+ offset 2)))
      ((31) (list (+ 65821 (mread-raw-unsigned mmdb offset 3)) (+ offset 3))))))

(defun mread-datafield-metadata (mmdb offset)
  (bind ((control-byte (mread-raw-uchar mmdb offset))
         ((type offset) (bind ((type0 (ash control-byte -5)))
                          (if (= type0 0)
                              (list (+ 7 (mread-raw-uchar mmdb (+ offset 1))) (+ offset 1))
                              (list type0 offset))))
         (length0 (logand control-byte #b11111)))
    (cons type
          (if (= type 1)
              (mread-special-pointer-length mmdb offset length0)
              (if (< length0 29)
                  (list length0 (+ offset 1))
                  (mread-special-generic-length mmdb offset length0))))))

(defun mread-pointer (mmdb offset length)
  (bind (((:structure maxmind-database- metadata) mmdb)
         ((:structure maxmind-database-metadata- search-tree-size) metadata)
         (control-byte (mread-raw-uchar mmdb offset))
         (length0 (logand control-byte #b11111))
         (size (ash length0 -3))
         (value (logand length0 #b111))
         (pointer-value (ecase size
                          ((0) (+ (ash value 8) (mread-raw-uchar mmdb (+ offset 1))))
                          ((1) (+ (ash value 16) (mread-raw-unsigned mmdb (+ offset 1) 2) 2048))
                          ((2) (+ (ash value 24) (mread-raw-unsigned mmdb (+ offset 1) 3) 526336))
                          ((3) (mread-raw-unsigned mmdb (+ offset 1) 4))))
         (data-offset (+ search-tree-size 16 pointer-value)))
    (values (mread-data mmdb data-offset)
            (+ offset length))))

(defun mread-bytes (mmdb offset length)
  (bind (((:structure maxmind-database- ptr) mmdb)
         (utf8-bytes (make-array length :element-type '(unsigned-byte 8)))
         (next-offset (+ offset length)))
    (iter (for i from offset below next-offset)
          (for utf8i from 0)
          (setf (aref utf8-bytes utf8i)
                (mem-ref ptr :uchar i)))
    (values utf8-bytes
            next-offset)))

(defun mread-utf8-string (mmdb offset length)
  (bind (((:values utf8-bytes next-offset) (mread-bytes mmdb offset length)))
    (values (octets-to-string utf8-bytes :encoding :utf-8)
            next-offset)))

(defun mread-double (mmdb offset length)
  (declare (ignore length))
  (bind (((:values doubleasint next-offset) (mread-unsigned mmdb offset 8)))
    (values (decode-float64 doubleasint) next-offset)))

(defun mread-uint16 (mmdb offset length)
  (mread-unsigned mmdb offset length))

(defun mread-uint32 (mmdb offset length)
  (mread-unsigned mmdb offset length))

(defun mread-int32 (mmdb offset length)
  (bind (((:values uvalue next-offset) (mread-uint32 mmdb offset length))
         (sign (ash uvalue -31))
         (value0 (logand uvalue (- (expt 2 31) 1)))
         (value (if (= 0 sign)
                    value0
                    (- value0 (expt 2 31)))))
    (values value next-offset)))

(defun mread-uint64 (mmdb offset length)
  (mread-unsigned mmdb offset length))

(defun mread-uint128 (mmdb offset length)
  (mread-unsigned mmdb offset length))

(defun mread-mapentry (mmdb offset)
  (bind (((:values key value-offset) (mread-data mmdb offset))
         ((:values value next-offset) (mread-data mmdb value-offset))
         (key-kw (intern (string-upcase (substitute #\- #\_ key)) "KEYWORD")))
    (values (cons key-kw value) next-offset)))

(defun mread-map (mmdb offset length)
  (iter (with next-offset = offset)
        (for i from 0 below length)
        (for (values entry next-key-offset) = (mread-mapentry mmdb next-offset))
        (setf next-offset next-key-offset)
        (collect entry into map)
        (finally (return (values map next-offset)))))

(defun mread-array (mmdb data-start length)
  (bind ((values-array (make-array length)))
    (iter (with next-offset = data-start)
          (for i from 0 below length)
          (for (values value next-value-offset) = (mread-data mmdb next-offset))
          (setf (aref values-array i) value)
          (setf next-offset next-value-offset)
          (finally (return (values values-array next-offset))))))

(defun mread-data-cache-container (mmdb data-start length)
  (declare (ignore mmdb data-start length))
  (error "data cache read occured"))

(defun mread-end-marker (mmdb data-start length)
  (declare (ignore mmdb length))
  (values :end-marker data-start))

(defun mread-boolean (mmdb data-start length)
  (declare (ignore mmdb))
  (values (ecase length
            ((0) nil)
            ((1) t))
          data-start))

(defun mread-float (mmdb offset length)
  (declare (ignore length))
  (bind (((:values floatasint next-offset) (mread-unsigned mmdb offset 4)))
    (values (decode-float32 floatasint) next-offset)))

(defun mread-data (mmdb offset)
  (bind (((type length data-start) (mread-datafield-metadata mmdb offset)))
    (ecase type
      ((1) (mread-pointer mmdb data-start length))
      ((2) (mread-utf8-string mmdb data-start length))
      ((3) (mread-double mmdb data-start length))
      ((4) (mread-bytes mmdb data-start length))
      ((5) (mread-uint16 mmdb data-start length))
      ((6) (mread-uint32 mmdb data-start length))
      ((7) (mread-map mmdb data-start length))
      ((8) (mread-int32 mmdb data-start length))
      ((9) (mread-uint64 mmdb data-start length))
      ((10) (mread-uint128 mmdb data-start length))
      ((11) (mread-array mmdb data-start length))
      ((12) (mread-data-cache-container mmdb data-start length))
      ((13) (mread-end-marker mmdb data-start length))
      ((14) (mread-boolean mmdb data-start length))
      ((15) (mread-float mmdb data-start length)))))

(defun mread-node (mmdb node-number bit)
  (bind (((:structure maxmind-database- metadata) mmdb)
         ((:structure maxmind-database-metadata- record-size) metadata)
         (node-size-in-bytes (/ (* record-size 2) 8))
         (node-offset (* node-size-in-bytes node-number))
         (basic-record-size (floor record-size 8)))
    (if (evenp node-size-in-bytes)
        (mread-raw-unsigned mmdb (+ node-offset (* bit basic-record-size))
                            basic-record-size)
        (bind ((basic-value (mread-raw-unsigned mmdb (+ node-offset
                                                        (* bit (+ basic-record-size 1)))
                                                basic-record-size))
               (center-byte (mread-raw-uchar mmdb (+ node-offset basic-record-size)))
               (high (if (= bit 0)
                         (ash center-byte -4)
                         (logand center-byte #b1111)))
               (shift-value (* basic-record-size 8)))
          (+ (ash high shift-value) basic-value)))))

(defun metadata-marker-p (mmdb offset)
  (bind (((:structure maxmind-database- ptr size) mmdb))
    (iter (for i from offset below size)
          (for mi from 0 below (length *metadata-marker*))
          (for sample-byte = (svref *metadata-marker* mi))
          (for data-byte = (mem-ref ptr :uchar i))
          (when (/= sample-byte data-byte)
            (return nil))
          (finally (return t)))))

(defun find-metadata-start (mmdb)
  (bind (((:structure maxmind-database- size) mmdb))
    (iter (for offset from (- size 1) downto (- size 1 (* 128 1024)))
          (when (metadata-marker-p mmdb offset)
            (return (+ offset (length *metadata-marker*))))
          (finally (error "MaxmindDatabase metadata not found")))))

(defun map-value (key map)
  (cdr (assoc key map)))

(defun read-metadata (mmdb offset)
  (bind ((metadata-map (mread-data mmdb offset))
         (binary-format-major-version
          (map-value :binary-format-major-version metadata-map))
         (binary-format-minor-version
          (map-value :binary-format-minor-version metadata-map))
         (build-epoch
          (map-value :build-epoch metadata-map))
         (database-type
          (map-value :database-type metadata-map))
         (description
          (map-value :description metadata-map))
         (ip-version
          (map-value :ip-version metadata-map))
         (languages
          (map-value :languages metadata-map))
         (node-count
          (map-value :node-count metadata-map))
         (record-size
          (map-value :record-size metadata-map))
         (search-tree-size (* node-count (/ (* record-size 2) 8))))
    (make-maxmind-database-metadata
     :binary-format-major-version binary-format-major-version
     :binary-format-minor-version binary-format-minor-version
     :build-epoch build-epoch
     :database-type database-type
     :description description
     :ip-version ip-version
     :languages languages
     :node-count node-count
     :record-size record-size
     :search-tree-size search-tree-size)))

(defun find-ip-record (mmdb ip-bits)
  (bind (((:structure maxmind-database- metadata) mmdb)
         ((:structure maxmind-database-metadata- node-count search-tree-size) metadata)
         (record-value (iter (with current-node = 0)
                             (for ip-bit in ip-bits)
                             (for next-node = (mread-node mmdb current-node ip-bit))
                             (setf current-node next-node)
                             (when (= current-node node-count)
                               (return nil))
                             (when (> current-node node-count)
                               (return current-node)))))
    (when record-value
      (bind ((data-offset (+ (- record-value node-count) search-tree-size))
             (data-map (mread-data mmdb data-offset)))
        data-map))))

(defmacro with-mmdb ((mmdb file) &body body)
  `(with-mmap (ptr fd size ,file)
     (bind ((,mmdb (make-maxmind-database :ptr ptr :filename ,file :size size))
            (metadata (read-metadata ,mmdb (find-metadata-start ,mmdb))))
       (setf (maxmind-database-metadata ,mmdb) metadata)
       ,@body)))

(defun binary-list (number num-of-bits)
  (iter (initially (setq q number))
        (for i from 0 below num-of-bits)
        (for (values q r) = (floor q 2))
        (collect r at beginning)))

(defun binary-128 (number)
  (binary-list number 128))

(defun binary-32 (number)
  (binary-list number 32))

(defun mmdb-query (mmdb ip-string)
  (bind (((:structure maxmind-database- metadata) mmdb)
         ((:structure maxmind-database-metadata- ip-version) metadata)
         ((ipa-version . ipa-value) (parse-ip ip-string)))
    (when (and (= ip-version 4)
               (= ipa-version 6))
      (error "Can't query ipv6 address in ipv4 database"))
    (bind ((ip-bits (ecase ip-version
                      ((4) (binary-32 ipa-value))
                      ((6) (binary-128 ipa-value)))))
          (find-ip-record mmdb ip-bits))))

(defun get-in (ip-record &rest path)
  (iter (for key in path)
        (for record initially ip-record then (map-value key record))
        (finally (return record))))
