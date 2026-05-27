#!/usr/bin/env -S sbcl --script

(let ((h (make-hash-table :test #'equal)))
  (with-open-file (s "passwd")
    (loop for l = (read-line s nil)
          while l
          for sh = (subseq l (1+ (position #\: l :from-end t)))
          do (incf (gethash sh h 0))))
  (maphash (lambda (k v)
             (format t "~a : ~a~%" k v))
           h))
