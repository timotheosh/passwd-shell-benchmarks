#!/usr/bin/guile -s
!#

(use-modules (ice-9 binary-ports)
             (rnrs bytevectors))

(define buffer-size (* 1024 1024))
(define max-shell-len 512)

;; entry = #(shell-bytevector count)

(define (shell-bytes=? tmp len shell)
  (and (= len (bytevector-length shell))
       (let loop ((i 0))
         (or (= i len)
             (and (= (bytevector-u8-ref tmp i)
                     (bytevector-u8-ref shell i))
                  (loop (+ i 1)))))))

(define (copy-shell tmp len)
  (let ((bv (make-bytevector len)))
    (bytevector-copy! tmp 0 bv 0 len)
    bv))

(define (count-shell tmp len table)
  (let ((bucket (hash-ref table len '())))
    (let search ((lst bucket))
      (cond
       ((null? lst)
        (hash-set!
         table
         len
         (cons (vector (copy-shell tmp len) 1)
               bucket)))

       ((shell-bytes=? tmp len
                       (vector-ref (car lst) 0))
        (vector-set!
         (car lst)
         1
         (+ 1 (vector-ref (car lst) 1))))

       (else
        (search (cdr lst)))))))

(define (main)

  (let ((table (make-hash-table))
        (buf   (make-bytevector buffer-size))
        (shell (make-bytevector max-shell-len)))
    (call-with-input-file
        "passwd"
      (lambda (port)
        (let ((colon-count 0)
              (shell-len 0)
              (capturing #f))
          (let chunk-loop ()
            (let ((n (get-bytevector-n! port
                                        buf
                                        0
                                        buffer-size)))
              (unless (eof-object? n)
                (let byte-loop ((i 0))
                  (when (< i n)
                    (let ((b (bytevector-u8-ref buf i)))
                      (cond
                       ;; :
                       ((= b 58)
                        (set! colon-count (+ colon-count 1))
                        (when (= colon-count 6)
                          (set! capturing #t)
                          (set! shell-len 0)))

                       ;; newline
                       ((= b 10)
                        (when (and capturing
                                   (> shell-len 0))
                          (count-shell shell
                                       shell-len
                                       table))
                        (set! colon-count 0)
                        (set! shell-len 0)
                        (set! capturing #f))

                       ;; shell byte
                       (capturing
                        (bytevector-u8-set!
                         shell
                         shell-len
                         b)
                        (set! shell-len
                              (+ shell-len 1))))
                      (byte-loop (+ i 1)))))
                (chunk-loop)))))))

    ;; output
    (hash-for-each
     (lambda (_ bucket)
       (for-each
        (lambda (entry)
          (display
           (utf8->string
            (vector-ref entry 0)))
          (display " : ")
          (display
           (vector-ref entry 1))
          (newline))
        bucket))
     table)))

(main)
