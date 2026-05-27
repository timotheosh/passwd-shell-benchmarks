#!sbcl --script
(defun inc (val)
  (declare
   (optimize (speed 3) (debug 0) (safety 0)))
  (if val
      (1+ val)
      1))

(defun split-passwd (file)
  (declare (type simple-string file)
           (optimize (speed 3) (debug 0) (safety 0)))
  (with-open-file (stream file :if-does-not-exist nil)
    (when stream
      (loop
        for line = (read-line stream nil)
        while line
        for brk-mark = (position-if (lambda (char) (char= #\: char)) line :from-end t)
        if brk-mark
          collect (subseq line (1+ brk-mark))))))

(defun occurrences (lst)
  (declare (type simple-list lst)
           (optimize (speed 3) (debug 0) (safety 0)))
  (let ((table (make-hash-table :size (length lst) :test #'equal)))
    (loop for shell in lst do
      (setf (gethash shell table) (inc (gethash shell table))))
    table))

(defun main (&rest argv)
  (declare (ignorable argv)
           (optimize (speed 3) (debug 0) (safety 0)))
  (loop for k being the hash-keys in (occurrences (split-passwd "passwd")) using (hash-value v)
        do (format t "~a : ~a~%" k v)))

(main)
