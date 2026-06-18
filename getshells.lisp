(declaim (optimize (speed 3) (safety 0) (debug 0)
                   (compilation-speed 0)))

(defconstant +buffer-size+ #.(* 1024 1024))
(defconstant +max-shell-len+ 512)

(deftype octet-buffer ()
  '(simple-array (unsigned-byte 8) (*)))

(defstruct shell-entry
  (name "" :type simple-string)
  (count 0 :type fixnum))

(defun shell-bytes= (tmp len str)
  (declare (type octet-buffer tmp)
           (fixnum len)
           (simple-string str))
  (and (= len (length str))
       (loop for i fixnum from 0 below len
             always (= (aref tmp i)
                       (char-code (schar str i))))))

(defun shell-string (tmp len)
  (declare (type octet-buffer tmp)
           (fixnum len))
  (let ((s (make-string len)))
    (declare (simple-string s))
    (loop for i fixnum from 0 below len
          do (setf (schar s i)
                   (code-char (aref tmp i))))
    s))

(defun count-shell (tmp len table)
  (declare (type octet-buffer tmp)
           (fixnum len)
           (hash-table table))
  (let ((bucket (gethash len table)))
    (dolist (entry bucket)
      (declare (type shell-entry entry))
      (when (shell-bytes= tmp len (shell-entry-name entry))
        (incf (shell-entry-count entry))
        (return-from count-shell nil)))
    (push (make-shell-entry :name (shell-string tmp len)
                            :count 1)
          (gethash len table))))

(defun main ()
  (let ((table (make-hash-table :test #'eql))
        (buf (make-array +buffer-size+
                         :element-type '(unsigned-byte 8)))
        (shell (make-array +max-shell-len+
                           :element-type '(unsigned-byte 8))))
    (declare (hash-table table)
             (type octet-buffer buf shell))

    (with-open-file (s "passwd"
                       :direction :input
                       :element-type '(unsigned-byte 8))
      (let ((colon-count 0)
            (shell-len 0)
            (capturing nil))
        (declare (fixnum colon-count shell-len))

        (loop for n fixnum = (read-sequence buf s)
              while (> n 0)
              do
                (loop for i fixnum from 0 below n
                      for b fixnum = (aref buf i)
                      do
                        (cond
                          ((= b 58) ; :
                           (incf colon-count)
                           (when (= colon-count 6)
                             (setf capturing t
                                   shell-len 0)))

                          ((= b 10) ; newline
                           (when (and capturing (> shell-len 0))
                             (count-shell shell shell-len table))
                           (setf colon-count 0
                                 shell-len 0
                                 capturing nil))

                          (capturing
                           (setf (aref shell shell-len) b)
                           (incf shell-len)))))))

    (maphash (lambda (_ bucket)
               (declare (ignore _))
               (dolist (entry bucket)
                 (format t "~A : ~D~%"
                         (shell-entry-name entry)
                         (shell-entry-count entry))))
             table)))
