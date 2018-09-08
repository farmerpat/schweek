;(load "schweek-loader.ss")
;(load "tiny-talk.sls")
;(load "schweek/matrix.ss")

(import (chezscheme)
        (schweek matrix)
        (tiny-talk)
        (srfi s64 testing))

(test-begin "matrix-test")

(define test-mat (new-matrix 1 1))

(test-assert "matrix-identity-1x1" (matrix? test-mat))

[$ mat-set! test-mat 0 0 'datum]

(test-eq "matrix-mat-set!-and-access-1x1-0,0" 'datum [$ mat-ref test-mat 0 0])
(test-error "matrix-out-of-range-access-throws-error-1x1" #t [$ mat-ref test-mat 1 1])

(set! test-mat (new-matrix 2 2 (lambda (elt) (symbol? elt)) 'datum))

(test-eq "matrix-default-values-fill-correctly-2x2-0,0" 'datum [$ mat-ref test-mat 0 0])
(test-eq "matrix-default-values-fill-correctly-2x2-0,1" 'datum [$ mat-ref test-mat 0 1])
(test-eq "matrix-default-values-fill-correctly-2x2-1,0" 'datum [$ mat-ref test-mat 1 0])
(test-eq "matrix-default-values-fill-correctly-2x2-1,1" 'datum [$ mat-ref test-mat 1 1])

[$ mat-set! test-mat 0 0 'pineapples]
(test-eq "matrix-mat-set!-and-access-2x2-0,0" 'pineapples [$ mat-ref test-mat 0 0])
[$ mat-set! test-mat 0 1 'peaches]
(test-eq "matrix-mat-set!-and-access-2x2-0,1" 'peaches [$ mat-ref test-mat 0 1])
[$ mat-set! test-mat 1 0 'pears]
(test-eq "matrix-mat-set!-and-access-2x2-1,0" 'pears [$ mat-ref test-mat 1 0])
[$ mat-set! test-mat 1 1 'plums]
(test-eq "matrix-mat-set!-and-access-2x2-1,1" 'plums [$ mat-ref test-mat 1 1])

(test-error "matrix-guard-throws-error-on-invalid-insertion-2x2" [$ mat-set! test-mat 0 0 3])

(test-assert "matrix-passes-guard-succeeds-on-valid-value-2x2" [$ passes-guard test-mat 'some-symbol])
(test-assert
  "matrix-passes-guard-fails-on-invalid-value-2x2"
  (not [$ passes-guard test-mat 3]))

(test-end "matrix-test")
