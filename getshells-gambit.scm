;;; Gambit Scheme implementation of getshells
;;; Compile to native binary: gsc -exe getshells-gambit.scm
;;;
;;; Key optimizations:
;;;   - Read entire file in one syscall; single forward pass over chars
;;;   - In-place shell comparison against alist avoids per-line allocations
;;;   - ##fx arithmetic and ##string-ref bypass runtime type checks (compiled)
;;;   - Alist beats hash table when there are only ~4 unique shells

(declare (standard-bindings) (extended-bindings) (block) (not safe))

;;; Compare content[start..end) with ref without allocating a substring
(define (slice=? content start end ref)
  (let ((rlen (##string-length ref)))
    (and (##fx= (##fx- end start) rlen)
         (let loop ((i 0))
           (if (##fx= i rlen)
               #t
               (and (##char=? (##string-ref content (##fx+ start i))
                              (##string-ref ref i))
                    (loop (##fx+ i 1))))))))

(define shells '())

;;; Slurp entire passwd file as one string
(define content
  (let ((port (open-input-file "passwd")))
    (let ((s (read-string 209715200 port)))  ; 200 MiB ceiling
      (close-input-port port)
      (if (eof-object? s) "" s))))

;;; Single forward scan: track last-colon position, act on newline
(let ((total (##string-length content)))
  (let loop ((i 0) (last-colon -1))
    (when (##fx< i total)
      (let ((c (##string-ref content i)))
        (cond
          ((##char=? c #\newline)
           (let ((shell-start (##fx+ last-colon 1)))
             (when (##fx> i shell-start)
               (let ((entry (let search ((lst shells))
                              (cond ((null? lst) #f)
                                    ((slice=? content shell-start i (##car (##car lst)))
                                     (##car lst))
                                    (else (search (##cdr lst)))))))
                 (if entry
                     (##set-cdr! entry (##fx+ (##cdr entry) 1))
                     (set! shells
                           (##cons (##cons (##substring content shell-start i) 1)
                                   shells)))))
             (loop (##fx+ i 1) -1)))
          ((##char=? c #\:)
           (loop (##fx+ i 1) i))
          (else
           (loop (##fx+ i 1) last-colon)))))))

(for-each
  (lambda (x)
    (display (##car x))
    (display " : ")
    (display (##cdr x))
    (newline))
  shells)
