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

(define (read-string port)
  (define (read-control-char)
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
  (read-char port)   ; toss #\"
  (let loop ((acc ""))
    (let ((c (read-char port)))
      (case c
        ((#\") acc) ; end of string
        ((#\\) (loop (string-append acc (read-control-char))))
        (else  (loop (string-append acc (string c))))))))

(define (read-array port)
  (read-char port)  ; toss #\[
  (let loop ((acc '()))
    (let ((c (peek-char port)))
      (case c
        ((#\,) (read-char port) (loop acc))
        ((#\]) (read-char port) acc)
        (else  (loop (append acc (list (read-json port))))))))) ; not a tail call :(

(define (read-json port)
  (let ((c (peek-char port)))
    (case c
      ((#\ht #\vt #\lf #\cr #\sp) ; skip whitespace
       (read-char port)
       (json->scm port))
      ((#\t) (read-true port))
      ((#\f) (read-false port))
      ((#\n) (read-null port))
      ((#\") (read-string port))
      ((#\[) (read-array port))
      ((#\{) (read-object port))
      (else  (read-number)))))

(define (json->scm port)
  (read-json port))

(display (json->scm (current-input-port)))
(newline)
