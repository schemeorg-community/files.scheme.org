;;; Run files.scm through the solver and checker and write the
;;; manifest to standard output.

(import (scheme base) (scheme file) (scheme write))
(import (only (gauche base) pprint))
(import (reader))

(define (main)
  (let ((files (with-input-from-file "files.scm" read-files)))
    (for-each (lambda (file)
                (write-string
                 (string-append (cadr (assoc 'sha1 file))
                                "  "
                                (cadr (assoc 'name file))
                                "\n")))
              files)))

(main)
