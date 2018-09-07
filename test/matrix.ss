;; do tests
;; need s64

(load "tiny-talk.sls")
(load "src/matrix.ss")

(import (chezscheme)
        (tiny-talk)
        (srfi s64 testing))

(test-begin "can-instantiate-unguarded-matrix")

(define test-mat (new-matrix 1 1))

(test-assert (matrix? test-mat))

(test-end "can-instantiate-unguarded-matrix")
