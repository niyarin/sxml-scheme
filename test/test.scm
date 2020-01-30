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
            => "<br/>")
          (check
            (sxml->xml-string
              '(WEIGHT (@ (unit "pound"))
                  (NET (@ (certified)) 67)
                  (GROSS 95)))
             => "<WEIGHT unit=\"pound\"><NET certified=\"certified\">67</NET><GROSS>95</GROSS></WEIGHT>")))))

(import (scheme base)
        (srfi 78)
        (niyarin test-sxml))


(test-sxml/sxml->xml-string)
(check-report)
