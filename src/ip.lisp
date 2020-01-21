;;;; -*- mode: lisp -*-

(in-package #:ru.bazon.cl-maxminddb)

(defun list->number (nls radix transformer)
  (iter (for m next (if m (* m radix) 1))
        (for n in nls)
        (sum (* m (funcall transformer n)))))

(defun char->number (char)
  (bind ((code (char-code char)))
    (cond
      ((and (>= code 48) (<= code 57)) (- code 48))
      ((and (>= code 97) (<= code 122)) (- code 87))
      ((and (>= code 65) (<= code 90)) (- code 55))
      (t (error "Unknown char: ~a" char)))))

(defun parse-ipv4 (ip-address)
  (bind ((grouped-form (iter (for p in-string ip-address)
                             (if (eq #\. p)
                                 (progn
                                   (collect cur-part into ip-parts at beginning)
                                   (setf cur-part '()))
                                 (collect p into cur-part at beginning))
                             (finally (return (cons cur-part ip-parts))))))
    (iter (for shift from 0 by 8)
          (for part-form in grouped-form)
          (for ip-part = (list->number part-form 10 #'char->number))
          (sum (ash ip-part shift)))))

(defun parse-ipv6 (ip-address)
  (bind ((grouped-form (iter (with state = :generic)
                             (for p in-string ip-address)
                             (case p
                               (#\: (if (eq :generic state)
                                        (progn
                                          (when cur-part
                                            (collect cur-part into ip-parts at beginning))
                                          (setf cur-part '())
                                          (setf state :ddot))
                                        (collect nil into ip-parts at beginning)))
                               (t (progn
                                    (setf state :generic)
                                    (collect p into cur-part at beginning))))
                             (finally (return (if cur-part
                                                  (cons cur-part ip-parts)
                                                  ip-parts))))))
    (iter (with gf-length = (length grouped-form))
          (for shift from 0 by 16)
          (for part-form in grouped-form)
          (if part-form
              (bind ((ip-part (list->number part-form 16 #'char->number)))
                (sum (ash ip-part shift)))
              (incf shift (* 16 (- 8 gf-length)))))))

(defun parse-ip (ip-address)
  (if (find #\. ip-address)
      (cons 4 (parse-ipv4 ip-address))
      (cons 6 (parse-ipv6 ip-address))))

