(import (scheme base) (scheme file) (scheme read) (scheme write))

(define (get-one valid? key alist)
  (let ((tail (cdr (or (assoc key alist) (error "Missing key" key)))))
    (if (and (= 1 (length tail)) (valid? (car tail)))
        (car tail)
        (error "Bad alist entry"))))

(define (get-string  key alist) (get-one string?  key alist))

(define (read-all)
  (let loop ((xs '()))
    (let ((x (read)))
      (if (eof-object? x) (reverse xs) (loop (cons x xs))))))

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
           (unless (memq (car x) '(meta))
             (display* "</" (car x) ">"))))
        ((string? x)
         (string-for-each display-char x))
        (else (error "Bad:" x))))

(define (display-page files)
  (display-sxml
   `(html
     (@ (lang "en"))
     (head
      (title "Scheme File Archive")
      (meta (@ (charset "UTF-8")))
      (style ""
        "body { font-family: sans-serif; }"))
     (body
      (h1 "Scheme File Archive")
      (p " Source is in a "
         (a (@ (href "https://github.com/schemeorg/files.scheme.org"))
            "git repository") ".")
      (table
       ,@(map (lambda (form)
                (when (eq? 'file (car form))
                  (let* ((file (cdr form))
                         (name (get-string 'name file)))
                    `(tr (td (kbd (a (@ (href ,name)) ,name)))
                         (td ,(get-string 'tagline file))))))
              files))))))

(define (main)
  (let ((files (with-input-from-file "files.scm" read-all)))
    (with-output-to-file "files/index.html"
      (lambda () (display-page files)))))

(main)
