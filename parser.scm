(use-modules (rnrs io ports))

(define (throw-invalid port)
  (throw 'json-invalid (get-line port)))

(define (expect-string port expected)
  (let ((found (get-string-n port (string-length expected))))
    (if (not (string=? found expected))
        (throw-invalid port))))

(define (read-true port)
  (expect-string port "true")
  #t)

(define (read-false port)
  (expect-string port "false")
  #f)

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
        (else  (throw-invalid port)))))
  (read-char port)   ; toss #\"
  (let loop ((acc ""))
    (let ((c (read-char port)))
      (case c
        ((#\") acc) ; end of string
        ((#\\) (loop (string-append acc (read-control-char))))
        (else  (loop (string-append acc (string c))))))))

(define (read-number port)
  (let loop ((acc ""))
    (let ((c (peek-char port)))
      (case c
        ((#\ht #\vt #\lf #\cr #\sp #\, #\} #\])  ; whitespace, other possible terminators
         (let ((result (string->number acc)))
           (if (eq? result #f)
               (throw-invalid port)
               result)))
        (else (loop(string-append acc (string (read-char port)))))))))


(define (read-array port)
  (read-char port)  ; toss #\[
  (let loop ((acc '()))
    (let ((c (peek-char port)))
      (case c
        ((#\ht #\vt #\lf #\cr #\sp)     ; TODO: how to not repeat this everywhere?
         (read-char port) (loop acc))
        ((#\,) (read-char port) (loop acc))
        ((#\]) (read-char port) acc)
        (else  (loop (append acc (list (read-json port)))))))))  ; not a tail call :(

(define (read-json port)
  (let ((c (peek-char port)))
    (case c
      ((#\ht #\vt #\lf #\cr #\sp) ; skip whitespace
       (read-char port)
       (read-json port))
      ((#\t) (read-true port))
      ((#\f) (read-false port))
      ((#\n) (read-null port))
      ((#\") (read-string port))
      ((#\[) (read-array port))
      ((#\{) (read-object port))
      (else  (read-number port)))))

(define (json->scm port)
  (read-json port))

(display (json->scm (current-input-port)))
(newline)
