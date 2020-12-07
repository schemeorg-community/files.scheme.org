;;; Run files.scm through the solver and checker and write the
;;; expanded version to standard output.

(import (scheme base) (scheme file) (scheme write))
(import (only (gauche base) pprint))
(import (reader))

(define (for-each/between visit between xs)
  (unless (null? xs)
    (visit (car xs))
    (let loop ((xs (cdr xs)))
      (unless (null? xs) (between) (visit (car xs)) (loop (cdr xs))))))

(define (main)
  (let ((files (with-input-from-file "files.scm" read-files)))
    (for-each/between pprint newline files)))

(main)
