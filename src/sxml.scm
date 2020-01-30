(define-library (niyarin sxml)
   (import
     (scheme base)
     (srfi 1) ;(scheme list)
     (scheme write);
     (scheme cxr))
   (export sxml->xml-string)

   (begin
     (define (%attribute-list? sxml)
       (and (list? sxml) (not (null? sxml)) (eq? (car sxml) '@)))

     (define (%top? sxml)
       (and (list? sxml) (eq? (car sxml) '*TOP*)))

     (define (%comment? sxml)
       (and (list? sxml) (eq? (car sxml) '*COMMENT*)))

     (define (%PI? sxml)
       (and (list? sxml) (eq? (car sxml) '*PI*)))

      (define (%element? sxml)
        (and (list? sxml) (not (null? sxml)) (symbol? (car sxml))))


     (define (%atom-convert sxml)
       (cond
         ((string? sxml) sxml)
         ((number? sxml) (number->string sxml))
         ((symbol? sxml) (symbol->string sxml))
         (else (error "invalid atom" sxml))))

     (define (sxml->xml-string sxml)
       (let loop ((sxml sxml))
          (cond
            ((string? sxml) sxml)
            ((number? sxml) (number->string sxml))
            ((%top? sxml)
               (apply string-append (map loop (cdr sxml))))
            ((%comment? sxml) "")
            ((%PI? sxml)
               (string-append
                 "<?"
                 (symbol->string (cadr sxml))
                 " "
                 (caddr sxml)
                 ">"))
            ((%element? sxml)
             (let* ((have-attribute
                      (and (not (null? (cdr sxml)))
                           (%attribute-list? (cadr sxml))))
                    (children (if have-attribute (cddr sxml) (cdr sxml)))
                    (attribute
                        (if have-attribute
                           (let loop ((attribute (cdadr sxml))
                                      (res ""))
                             (if  (null? attribute)
                               res
                               (loop
                                 (cdr attribute)
                                 (string-append
                                     res
                                     " "
                                     (symbol->string (caar  attribute))
                                     "="
                                     "\""
                                     (if (null? (cdar attribute))
                                       (%atom-convert (caar attribute))
                                       (%atom-convert (cadar attribute)))
                                     "\""))))
                           "")))

                   (if (null? children)
                     (string-append
                       "<"
                      (symbol->string  (car sxml))
                      attribute
                      "/>")

                    (string-append
                      "<"
                      (symbol->string  (car sxml))
                      attribute
                      ">"
                      (apply string-append (map loop (if have-attribute (cddr sxml) (cdr sxml))))
                      "</"
                      (symbol->string  (car sxml))
                      ">"))))
            (else
              (error "ERROR:invalid sxml" sxml)))))
     (define (xml-string->sxml))))
