(use-modules (rnrs io ports))

;; (define (expect-string stream expected-str)
;;   (let ((expected (string->list expected-str)))
;;     (if (not (null? expected))
;;         (let ((c (read-char stream)))
;;           (if (char=? c (car expected))
;;               (expect-string stream (list->string (cdr expected)))
;;               (throw 'json-invalid stream))))))


(define (expect-string stream expected)
  (let ((found (get-string-n stream (string-length expected))))
    (if (not (string=? found expected))
        (throw 'json-invalid stream))))


(define (read-true stream)
  (expect-string stream "true")
  #t)

(define (read-false stream)
  (expect-string stream "false")
  #t)

(define (read-null stream)
  (expect-string stream "null")
  #nil)

(define (read-control-char stream)
  ; TODO
  (throw 'not-implemented "control chars"))

(define (read-string stream)
  (read-char stream) ; toss #\"
  (let loop ((acc ""))
    (let ((c (read-char stream)))
      (case c
        ((#\") acc)
        ((#\\) (read-control-char stream))
        (else (loop (string-append acc (string c))))))))

(define (json->scm stream)
  (let ((c (peek-char stream)))
    (case c
      ((#\ht #\vt #\lf #\cr #\sp)
       (peak-char stream)
       (json->scm stream))
      ((#\t) (read-true stream))
      ((#\f) (read-false stream))
      ((#\n) (read-null stream))
      ((#\") (read-string stream))
      ((#\{) (read-object stream))
      ((#\[) (read-list stream))
      (else  (read-number)))))


(display (json->scm (current-input-port)))
(newline)
