#!/usr/bin/guile -s
!#

(use-modules (ice-9 rdelim))

(define shells (make-hash-table))

(call-with-input-file
    "passwd"
  (lambda (port)
    (let loop ((line (read-line port)))
      (unless (eof-object? line)

        (let* ((idx   (string-rindex line #\:))
               (shell (substring line (+ idx 1))))

          (hash-set! shells
                     shell
                     (+ 1 (hash-ref shells shell 0))))

        (loop (read-line port))))))

(hash-for-each
 (lambda (shell count)
   (format #t "~a : ~a~%" shell count))
 shells)
