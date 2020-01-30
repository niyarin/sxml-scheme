(include "../src/sxml.scm")

(define-library (niyarin test-sxml)
   (import (scheme base)
           (srfi 78)
           (scheme write);
           (niyarin sxml))
   (export test-sxml/sxml->xml-string)
   (begin
     (define (test-sxml/sxml->xml-string)
       (begin
          (check
            (sxml->xml-string
              '(br))
            => "<br/>")))))

(import (scheme base)
        (srfi 78)
        (niyarin test-sxml))


(test-sxml/sxml->xml-string)
(check-report)
