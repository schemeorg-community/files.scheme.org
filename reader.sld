(define-library (reader)
  (export read-files)
  (import (scheme base) (scheme read) (srfi 1))
  (import (solver))
  (begin

    (define (list->pair list)
      (if (= 2 (length list))
          (cons (car list) (cadr list))
          (error "Malformed entry")))

    (define (pair->list pair) (list (car pair) (cdr pair)))

    (define (read-all)
      (let loop ((forms '()))
        (let ((form (read)))
          (if (eof-object? form) (reverse forms) (loop (cons form forms))))))

    (define (read-files/raw)
      (map cdr (filter (lambda (form)
                         (and (pair? form) (eq? 'file (car form))))
                       (read-all))))

    (define (read-files)
      (let ((files (read-files/raw)))
        (map (lambda (file)
               (map pair->list (solve (map list->pair file))))
             files)))))
