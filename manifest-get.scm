;; ./manifest-get.sh size
;; ./manifest-get.sh sha1

(import (scheme base) (scheme file) (scheme write)
        (srfi 1) (srfi 13) (srfi 193))
(import (reader))

(define (main)
  (unless (= 1 (length (command-args)))
    (error "Usage"))
  (let ((which (string->symbol (first (command-args)))))
    (dump which)))

(define (dump which)
  (let* ((files (with-input-from-file "files.scm" read-files))
         (props (fold (lambda (file props)
                        (let ((entry (assoc which file)))
                          (if (not entry)
                              props
                              (let ((name (second (assoc 'name file)))
                                    (prop (second entry)))
                                (cons (list prop name)
                                      props)))))
                      '()
                      files))
         (maxlen (fold max 0 (map string-length (map first props)))))
    (for-each (lambda (prop)
                (write-string
                 (string-append
                  (string-pad (first prop) maxlen)
                  "  "
                  (second prop)
                  "\n")))
              props)))

(main)
