(define-library (solver)
  (export solve)
  (import (scheme base) (srfi 1))
  (begin

    (define (split whole)
      (let ((n (string-length whole)))
        (let outer ((parts '()) (a 0) (b 0))
          (cond ((= a b n)
                 (reverse parts))
                ((= b n)
                 (outer (cons (substring whole a b) parts) b b))
                ((char=? #\} (string-ref whole b))
                 (error "Stray }"))
                ((char=? #\{ (string-ref whole b))
                 (let inner ((parts (if (= a b)
                                        parts
                                        (cons (substring whole a b) parts)))
                             (a (+ b 1))
                             (b (+ b 1)))
                   (cond ((= b n)
                          (error "Unterminated {"))
                         ((not (char=? #\} (string-ref whole b)))
                          (inner parts a (+ b 1)))
                         ((= a b)
                          (error "No variable name inside {}"))
                         (else
                          (outer (cons (string->symbol (substring whole a b))
                                       parts)
                                 (+ b 1)
                                 (+ b 1))))))
                (else (outer parts a (+ b 1)))))))

    (define (stringify x)
      (cond ((string? x) x)
            ((number? x) (number->string x))
            (else "Huh?")))

    (define (solve-for key alist)
      (let recurse ((key key) (stack '()))
        (if (member key stack)
            (error "Solver loop" key)
            (fold (lambda (part newval)
                    (string-append
                     newval
                     (cond ((string? part) part)
                           ((symbol? part) (recurse part (cons key stack)))
                           (else (error "Huh?")))))
                  ""
                  (split (stringify
                          (cdr (or (assoc key alist)
                                   (error "Not defined" key)))))))))

    (define (solve alist)
      (map (lambda (pair) (cons (car pair) (solve-for (car pair) alist)))
           alist))))
