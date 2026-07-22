(require :sb-posix)

(declaim (optimize (speed 3) (safety 0) (debug 0)
                   (compilation-speed 0)))

;; --- libc SIMD bindings ---------------------------------------------------
;;
;; memchr is POSIX and present on every target libc (glibc, musl, BSD libc,
;; illumos libc). memrchr is a GNU extension on Linux (glibc) but is also
;; available on FreeBSD libc (added in FreeBSD 11). It is absent on OpenBSD
;; and illumos libc. We bind it conditionally: native memrchr on Linux and
;; FreeBSD (fastest path, single backward SIMD scan), and a portable
;; forward-memchr-chain fallback everywhere else.
(sb-alien:define-alien-routine ("memchr" %memchr) sb-alien:unsigned-long
  (addr sb-alien:unsigned-long)
  (c    sb-alien:int)
  (n    sb-alien:size-t))

#+(or linux freebsd)
(sb-alien:define-alien-routine ("memrchr" %memrchr) sb-alien:unsigned-long
  (addr sb-alien:unsigned-long)
  (c    sb-alien:int)
  (n    sb-alien:size-t))

;; MADV_SEQUENTIAL is 2 on Linux, FreeBSD, OpenBSD, and illumos alike
;; (shared BSD-derived mman.h numbering), so no per-OS branching needed.
(sb-alien:define-alien-routine ("madvise" %madvise) sb-alien:int
  (addr   sb-alien:unsigned-long)
  (length sb-alien:size-t)
  (advice sb-alien:int))

(defconstant +madv-sequential+ 2)

;; -------------------------------------------------------------------------

(defstruct (shell-entry (:conc-name se-))
  (name  ""  :type simple-string)
  (count  0  :type fixnum))

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

;; Find the index of the last occurrence of CH in [start, end), or -1.
;;
;; #+(or linux freebsd): single native memrchr call (SIMD backward scan).
;; #+sunos: backward byte scan via sap-ref-8 — single O(n) pass with no
;;   per-iteration FFI overhead; SBCL (speed 3) compiles the loop to a
;;   tight register loop.  Faster than the forward chain on illumos whose
;;   libc lacks memrchr.
;; fallback: forward memchr chain for everything else (e.g. OpenBSD).
#+(or linux freebsd)
(declaim (inline last-index-of))
#+(or linux freebsd)
(defun last-index-of (base start end ch)
  (declare (type sb-vm:word base)
           (fixnum start end)
           (fixnum ch))
  (let* ((len  (- end start))
         (hit  (%memrchr (+ base start) ch len)))
    (declare (fixnum len) (type sb-vm:word hit))
    (if (zerop hit) -1 (the fixnum (- hit base)))))

#+sunos
(declaim (inline last-index-of))
#+sunos
(defun last-index-of (base start end ch)
  (declare (type sb-vm:word base)
           (fixnum start end)
           (fixnum ch))
  (let ((sap (sb-sys:int-sap base))
        (i   (1- end)))
    (declare (fixnum i))
    (loop
      (when (< i start) (return -1))
      (when (= (sb-sys:sap-ref-8 sap i) ch) (return i))
      (decf i))))

#-(or linux freebsd sunos)
(declaim (inline last-index-of))
#-(or linux freebsd sunos)
(defun last-index-of (base start end ch)
  (declare (type sb-vm:word base)
           (fixnum start end)
           (fixnum ch))
  (let ((pos start)
        (last -1))
    (declare (fixnum pos last))
    (loop
      (when (>= pos end) (return))
      (let ((hit (%memchr (+ base pos) ch (- end pos))))
        (declare (type sb-vm:word hit))
        (when (zerop hit) (return))
        (setf last (the fixnum (- hit base)))
        (setf pos (1+ last))))
    last))

(defun main ()
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

            (let* ((nl-addr  (%memchr (+ base line-start) 10 (- size line-start)))
                   (nl-pos   (if (zerop nl-addr)
                                 size
                                 (the fixnum (- nl-addr base)))))
              (declare (fixnum nl-pos) (type sb-vm:word nl-addr))

              (let ((colon-pos (last-index-of base line-start nl-pos 58)))
                (declare (fixnum colon-pos))

                (unless (= colon-pos -1)
                  (let ((sh-start (1+ colon-pos)))
                    (declare (fixnum sh-start))
                    (when (< sh-start nl-pos)
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

      (sb-posix:munmap ptr size)
      (sb-posix:close fd))

    (dolist (e shells)
      (format t "~A : ~D~%" (se-name e) (se-count e)))))
