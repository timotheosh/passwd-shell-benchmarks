#!/usr/bin/guile -s
!#

(let ((h '()))
  (call-with-input-file "passwd"
    (lambda (p)
      (let loop ((c (read-char p)) (s ""))
        (if (eof-object? c)
            #t
            (if (char=? c #\newline)
                (begin
                  (let* ((i (let f ((n (- (string-length s) 1)))
                              (if (char=? (string-ref s n) #\:)
                                  n
                                  (f (- n 1)))))
                         (sh (substring s (+ i 1) (string-length s)))
                         (x (assoc sh h)))
                    (if x
                        (set-cdr! x (+ 1 (cdr x)))
                        (set! h (cons (cons sh 1) h))))
                  (loop (read-char p) ""))
                (loop (read-char p)
                      (string-append s (string c))))))))
  (for-each
    (lambda (x)
      (display (car x))
      (display " : ")
      (display (cdr x))
      (newline))
    h))
