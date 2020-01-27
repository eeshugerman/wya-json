(use-modules (rnrs io ports))


(define (expect-string port expected)
  (let ((found (get-string-n port (string-length expected))))
    (if (not (string=? found expected))
        (throw 'json-invalid port))))

(define (read-true port)
  (expect-string port "true")
  #t)

(define (read-false port)
  (expect-string port "false")
  #t)

(define (read-null port)
  (expect-string port "null")
  #nil)

(define (read-control-char port)
  (let ((c (read-char port)))
    (case c
      ((#\" #\\ #\/) (string c))
      ((#\b) (string #\bs))
      ((#\f) (string #\ff))
      ((#\n) (string #\lf))
      ((#\r) (string #\cr))
      ((#\t) (string #\ht))
      ((#\u) (throw 'not-implemented "escaped unicode characters"))
      (else  (throw 'json-invalid port)))))

(define (read-string port)
  (read-char port)   ; toss #\"
  (let loop ((acc ""))
    (let ((c (read-char port)))
      (case c
        ((#\") acc)
        ((#\\) (loop (string-append acc (read-control-char port))))
        (else  (loop (string-append acc (string c))))))))

(define (json->scm port)
  (let ((c (peek-char port)))
    (case c
      ((#\ht #\vt #\lf #\cr #\sp)
       (peak-char port)
       (json->scm port))
      ((#\t) (read-true port))
      ((#\f) (read-false port))
      ((#\n) (read-null port))
      ((#\") (read-string port))
      ((#\{) (read-object port))
      ((#\[) (read-list port))
      (else  (read-number)))))


(display (json->scm (current-input-port)))
(newline)
