(declaim (optimize (speed 3) (safety 0) (debug 0)
                   (compilation-speed 0)))

;; --- glibc SIMD bindings -------------------------------------------------
;;
;; Passing addresses as sb-alien:unsigned-long avoids boxing a SAP heap
;; object on every call, which eliminates 32 bytes of consing per line.
(sb-alien:define-alien-routine ("memchr" %memchr) sb-alien:unsigned-long
  (addr sb-alien:unsigned-long)
  (c    sb-alien:int)
  (n    sb-alien:size-t))

(sb-alien:define-alien-routine ("memrchr" %memrchr) sb-alien:unsigned-long
  (addr sb-alien:unsigned-long)
  (c    sb-alien:int)
  (n    sb-alien:size-t))

;; MADV_SEQUENTIAL (2) tells the kernel to read-ahead pages aggressively.
;; Without it, mmap access triggers individual minor page faults one-by-one;
;; with it, the kernel pipelines them and the scan completes without stalls.
(sb-alien:define-alien-routine ("madvise" %madvise) sb-alien:int
  (addr   sb-alien:unsigned-long)
  (length sb-alien:size-t)
  (advice sb-alien:int))

(defconstant +madv-sequential+ 2)

;; -------------------------------------------------------------------------

(defstruct (shell-entry (:conc-name se-))
  (name  ""  :type simple-string)
  (count  0  :type fixnum))

;; Compare mmap bytes [sh-start, sh-end) against stored shell name.
;; Inlined so the SAP stays in a register and sap-ref-8 compiles to
;; a single MOV instruction with no heap allocation.
(declaim (inline match-shell))
(defun match-shell (base sh-start sh-end name)
  (declare (type sb-sys:system-area-pointer base)
           (fixnum sh-start sh-end)
           (simple-string name))
  (let ((len (- sh-end sh-start)))
    (declare (fixnum len))
    (and (= len (length name))
         (loop for i fixnum from 0 below len
               always (= (sb-sys:sap-ref-8 base (+ sh-start i))
                         (char-code (schar name i)))))))

;; Materialise a shell name string from the mmap region.
;; Called at most once per distinct shell (typically 3-6 times total).
(defun materialize (base start end)
  (declare (type sb-sys:system-area-pointer base)
           (fixnum start end))
  (let* ((len (- end start))
         (s   (make-string len)))
    (declare (fixnum len) (simple-string s))
    (loop for i fixnum from 0 below len
          do (setf (schar s i)
                   (code-char (sb-sys:sap-ref-8 base (+ start i)))))
    s))

(defun main ()
  ;; mmap gives us a zero-copy view of the file.  The OS maps pages from
  ;; the page cache; MADV_SEQUENTIAL causes it to pre-fetch ahead so the
  ;; forward scan never stalls waiting for individual page faults.
  (let* ((fd    (sb-posix:open "passwd" sb-posix:o-rdonly 0))
         (size  (sb-posix:stat-size (sb-posix:fstat fd)))
         (ptr   (sb-posix:mmap nil size
                               sb-posix:prot-read
                               sb-posix:map-private
                               fd 0))
         (base  (sb-sys:sap-int ptr))
         (shells '()))
    (declare (fixnum size) (type sb-vm:word base))
    (%madvise base size +madv-sequential+)

    (unwind-protect
        (let ((line-start 0))
          (declare (fixnum line-start))
          (loop
            (when (>= line-start size) (return))

            ;; memchr / memrchr use AVX2/SSE4.2 under the hood:
            ;; both are dramatically faster than any byte-at-a-time Lisp loop.
            (let* ((nl-addr  (%memchr (+ base line-start) 10 (- size line-start)))
                   (nl-pos   (if (zerop nl-addr)
                                 size
                                 (the fixnum (- nl-addr base)))))
              (declare (fixnum nl-pos) (type sb-vm:word nl-addr))

              (let* ((line-len    (- nl-pos line-start))
                     (colon-addr (%memrchr (+ base line-start) 58 line-len)))
                (declare (fixnum line-len) (type sb-vm:word colon-addr))

                (unless (zerop colon-addr)
                  (let ((sh-start (1+ (the fixnum (- colon-addr base)))))
                    (declare (fixnum sh-start))
                    (when (< sh-start nl-pos)
                      ;; Linear search over the small set of distinct shells.
                      (let ((found nil))
                        (dolist (e shells)
                          (declare (type shell-entry e))
                          (when (match-shell ptr sh-start nl-pos (se-name e))
                            (incf (se-count e))
                            (setf found t)
                            (return)))
                        (unless found
                          (push (make-shell-entry
                                 :name  (materialize ptr sh-start nl-pos)
                                 :count 1)
                                shells)))))))

              (setf line-start (1+ nl-pos)))))

      ;; Always release the mapping and file descriptor.
      (sb-posix:munmap ptr size)
      (sb-posix:close fd))

    (dolist (e shells)
      (format t "~A : ~D~%" (se-name e) (se-count e)))))
