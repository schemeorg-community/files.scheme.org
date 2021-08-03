(import (scheme base) (scheme file) (scheme write))
(import (reader))

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

(define (menu-sxml items)
  `(header
    (ul (@ (class "menu"))
        ,@(map (lambda (i) `(li (a (@ (href ,(cadr i))) ,(car i))))
               items))))

(define (about-section)
  `(section
    (h2 "About files.scheme.org")
    (div (@ (id "schemeorg-contributing")
            (class "round-box green-box"))
         (p (kbd "files.scheme.org")
            " is a community subdomain of "
            (kbd "scheme.org") ".")
         (ul
          (li "Source code: "
              (a (@ (href
                     "https://github.com/schemeorg/files.scheme.org"))
                 (kbd (@ (class "github-repo"))
                      "schemeorg/files.scheme.org"))
              " repository on GitHub.")
          (li "Discussion: "
              (code (@ (class "mailing-list"))
                    "schemeorg")
              " mailing list ("
              (a (@ (href
                     "https://srfi-email.schemers.org/schemeorg/"))
                 "archives")
              ", "
              (a (@ (href
                     ,(string-append
                       "https://srfi.schemers.org/"
                       "srfi-list-subscribe.html#schemeorg")))
                 "subscribe")
              ").")))))

(define (display-page files)
  (display-sxml
   (let ((title "Scheme Files")
         (description
          (string-append
           "Download archive of Scheme programming implementations,"
           " libraries, documents and other files.")))
     `(html
       (@ (lang "en"))
       (head
        (meta (@ (charset "UTF-8")))
        (title ,title)
        (link (@ (rel "stylesheet") (href "/schemeorg.css")))
        (link (@ (rel "stylesheet") (href "/files.css")))
        (meta (@ (name "viewport")
                 (content "width=device-width, initial-scale=1")))
        (meta (@ (name "description")
                 (content ,description))))
       (body
        ,(menu-sxml
          '(("Home" "https://scheme.org/")
            ("Docs" "https://doc.scheme.org/")
            ("Community" "https://community.scheme.org/")
            ("Standards" "https://standards.scheme.org/")
            ("Implementations" "https://implementations.scheme.org/")))
        (h1 (@ (id "logo"))
            ,title)
        (table
         (@ (class "files"))
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
            files))
        ,(about-section))))))

(define (main)
  (let ((files (with-input-from-file "files.scm" read-files)))
    (with-output-to-file "www/index.html"
      (lambda () (display-page files)))))

(main)
