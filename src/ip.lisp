;;;; -*- mode: lisp -*-

(in-package #:ru.bazon.cl-maxminddb)

(defun list->number (nls radix transformer)
  (iter (for m next (if m (* m radix) 1))
        (for n in nls)
        (sum (* m (funcall transformer n)))))

(defun char->number (char)
  (- (char-code char) 48))

(defun parse-ipv4 (ip-address)
  (bind ((grouped-form (iter (for p in-string ip-address)
                             (if (eq #\. p)
                                 (progn
                                   (collect cur-part into ip-parts at beginning)
                                   (setf cur-part '()))
                                 (collect p into cur-part at beginning))
                             (finally (return (cons cur-part ip-parts))))))
    (iter (for part-form in grouped-form)
          (for ip-part = (list->number part-form 10 #'char->number))
          (for shift from 0 by 8)
          (sum (ash ip-part shift)))))

(defun parse-ipv6 (ip-address)
  )

(defun parse-ip (ip-address)
  (if (find #\. ip-address)
      (cons 4 (parse-ipv4 ip-address))
      (cons 6 (parse-ipv6 ip-address))))

