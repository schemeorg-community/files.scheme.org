(import (scheme base) (scheme file) (scheme write))
(import (reader))

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
    (if entry (get-one-from-entry valid? entry) (error "Missing key" key))))

(define (get-string key alist)
  (get-one string? key alist))

(define (get-string-optional key alist)
  (get-one-optional string? key alist))

(define (map/odd f xs)
  (let loop ((acc '()) (xs xs) (odd? #f))
    (if (null? xs) (reverse acc)
        (loop (cons (f (car xs) odd?) acc) (cdr xs) (not odd?)))))

(define (superscripts s)
  (let ((n (string-length s)))
    (let loop ((a 0) (b 0) (acc '()))
      (cond ((= n b a)
             (reverse acc))
            ((= n b)
             (loop b b (cons (substring s a b) acc)))
            ((char=? #\^ (string-ref s b))
             (loop (+ b 2) (+ b 2)
                   (append `((sup ,(string (string-ref s (+ b 1))))
                             ,(substring s a b))
                           acc)))
            (else (loop a (+ b 1) acc))))))

(define (display-sxml x)
  (define (display* . xs) (for-each display xs))
  (define (display-char char)
    (let* ((cc (char->integer char))
           (ok? (case char ((#\& #\< #\> #\") #f) (else (<= #x20 cc #x7e)))))
      (if ok? (display char) (display* "&#" cc ";"))))
  (define (display-attribute attribute)
    (display* " " (car attribute) "=\"")
    (string-for-each display-char (cadr attribute))
    (display "\""))
  (cond ((pair? x)
         (display* "<" (car x))
         (let ((body (cond ((and (pair? (cdr x))
                                 (pair? (cadr x))
                                 (eq? '@ (car (cadr x))))
                            (for-each display-attribute (cdr (cadr x)))
                            (cddr x))
                           (else (cdr x)))))
           (display ">")
           (for-each display-sxml body)
           (unless (memq (car x) '(br meta))
             (display* "</" (car x) ">"))))
        ((string? x)
         (string-for-each display-char x))
        (else (error "Bad:" x))))

(define (display-page files)
  (display-sxml
   `(html
     (@ (lang "en"))
     (head
      (title "Scheme Files")
      (meta (@ (charset "UTF-8")))
      (style ""
        "body { font-family: sans-serif; }"
        "td { vertical-align: top; }"
        ".even { background-color: #dde; }"
        ".odd { }"
        ".note { color: teal; }"))
     (body
      (h1 "Scheme Files")
      (p " Source is in a "
         (a (@ (href "https://github.com/schemeorg/files.scheme.org"))
            "git repository") ".")
      (table
       ,@(map/odd
          (lambda (file odd?)
            (let ((name (get-string 'name file)))
              `(tr (@ (class ,(if odd? "odd" "even")))
                   (td (kbd (a (@ (href ,name)) ,name)))
                   (td ,@(superscripts (get-string 'title file))
                       ,@(let ((note (get-string-optional 'note file)))
                           (if (not note) '()
                               `((br)
                                 (span (@ (class "note"))
                                       "("
                                       ,@(superscripts note)
                                       ")"))))))))
          files))))))

(define (main)
  (let ((files (with-input-from-file "files.scm" read-files)))
    (with-output-to-file "files/index.html"
      (lambda () (display-page files)))))

(main)
