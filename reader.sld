(define-library (reader)
  (export get-string get-string-optional read-files)
  (import (scheme base) (scheme char) (scheme read) (srfi 1))
  (import (solver))
  (begin

    (define (get-one-from-entry valid? entry)
      (let ((tail (cdr entry)))
        (if (and (= 1 (length tail)) (valid? (car tail)))
            (car tail)
            (error "Bad alist entry"))))

    (define (get-one-optional valid? key alist)
      (let ((entry (assoc key alist)))
        (if entry (get-one-from-entry valid? entry) #f)))

    (define (get-one valid? key alist)
      (let ((entry (assoc key alist)))
        (if entry
            (get-one-from-entry valid? entry)
            (error "Missing key" key))))

    (define (get-string key alist)
      (get-one string? key alist))

    (define (get-string-optional key alist)
      (get-one-optional string? key alist))

    (define (version-string-split s)
      (define (split-off a b fields)
        (if (= a b) fields (cons (substring s a b) fields)))
      (let loop ((a 0) (b 0) (fields '()))
        (cond ((= b (string-length s))
               (reverse (split-off a b fields)))
              ((or (char=? #\. (string-ref s b))
                   (char=? #\- (string-ref s b))
                   (char=? #\_ (string-ref s b)))
               (loop b (+ b 1) (split-off a b fields)))
              (else
               (loop a (+ b 1) fields)))))

    (define (string-list-compare a b)
      (cond ((null? a) (if (null? b) 0 -1))
            ((null? b) 1)
            ((string-ci<? (car a) (car b)) -1)
            ((string-ci>? (car a) (car b)) 1)
            (else (string-list-compare (cdr a) (cdr b)))))

    (define known-extensions '(".gz" ".pdf" ".tar" ".tgz" ".zip"))

    (define (remove-ext lis)
      (let loop ((lis (reverse lis)))
        (if (and (not (null? lis)) (member (car lis) known-extensions))
            (loop (cdr lis))
            (reverse lis))))

    (define (version-string-compare a b)
      (string-list-compare (remove-ext (version-string-split a))
                           (remove-ext (version-string-split b))))

    (define (assert-version-order names)
      (or (null? names)
          (let loop ((a (car names)) (bs (cdr names)))
            (if (null? bs) #f
                (let ((b (car bs)))
                  (cond ((string-ci=? a b)
                         (error "Filenames are duplicate" a b))
                        ((> (version-string-compare a b) 0)
                         (error "Filenames are not in alphabetical order"
                                a b))
                        (else (loop b (cdr bs)))))))))

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
      (let ((files (map (lambda (file)
                          (map pair->list (solve (map list->pair file))))
                        (read-files/raw))))
        (let ((names (map (lambda (file) (get-string 'name file)) files)))
          (assert-version-order names))
        files))))
