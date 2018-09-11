(library (schweek matrix)
  (export
    new-matrix
    matrix?)

  (import
    (chezscheme)
    (tiny-talk))

(define new-matrix
  (case-lambda
    [(nrows ncols)
     (new-matrix nrows ncols (lambda (x) #t) 0)]
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
       (object ([rows nrows] [cols ncols] [guard guard])
         [(matrix? self) #t]
         [(mat-ref self row col)
          (vector-ref (vector-ref internal-rep row) col)]
         [(passes-guard self val)
          ([$ guard self] val)]
         [(mat-set! self row col val)
          (when (not [$ passes-guard self val])
            (error "matrix:mat-set!" "value did not pass guard" val))
          (vector-set! (vector-ref internal-rep row) col val)]))]))

(define-predicate matrix?))
