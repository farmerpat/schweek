(load "tiny-talk.sls")

(import (chezscheme)
        (tiny-talk))

(define new-matrix
  (case-lambda
    [(nrows ncols)
     (new-matrix nrows ncols (lambda (x) #t))]
    [(nrows ncols guard)
     (new-matrix nrows ncols guard 0)]
    [(nrows ncols guard filler)
     (when (not (and (number? nrows) (number? ncols)))
       (error "new-matrix" "invalid nrows/ncols" nrows ncols))
     (when (not (and (> nrows 0) (> ncols 0)))
       (error "new-matrix" "nrows/ncols <=0" nrows ncols))
     (let ((internal-rep (make-vector nrows)))
       (do ((i 0  (add1 i)))
           ((= i nrows))
           (vector-set!
             internal-rep
             i
             (make-vector ncols filler)))
       (object ([rows nrows] [cols ncols])
         [(matrix? self) #t]
         [(mat-ref self row col)
          (vector-ref (vector-ref internal-rep row) col)]
         [(mat-set! self row col val)
          (vector-set! (vector-ref internal-rep row) col val)])
       )]
    )
  )

(define-predicate matrix?)
