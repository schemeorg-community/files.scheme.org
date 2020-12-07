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

    (define (wrong-order <= lis)
      (or (null? lis)
          (let loop ((a (car lis)) (bs (cdr lis)))
            (if (null? bs) #f
                (let ((b (car bs)))
                  (if (<= b a) (list a b) (loop b (cdr bs))))))))

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
        (let* ((names (map (lambda (file) (get-string 'name file)) files))
               (wrong (wrong-order string-ci<=? names)))
          (when wrong
            (if (string=? (car wrong) (cadr wrong))
                (error "Filenames are duplicate" wrong)
                (error "Filenames are not in alphabetical order" wrong))))
        files))))
