;; NOTE:
;; on approach would be to require the definition
;; of a set of primitives, and to compose more
;; complicated shapes from them. with point and line,
;; everything else could be created (though less efficiently,
;; i'd think) without specific sdl calls (aside from those
;; required by pt and line).

;; if we hack add-method! etc, it may become possible to save
;; world state via serialization. we would have to save
;; the added lambdas making up the added serializable methods.

;; i sort of see each world as a collection of widgets/objects
;; and a scheme environment. maybe there is some way to
;; monkey with all that w/o hacking the scheme...
;; it seems like there isn't...

;; can't we load this conditionally?
;(load "tiny-talk.sls")

(import (chezscheme)
        (sdl2)
        (tiny-talk))

(define (new-world renderer)
  (let ((av (new-av-interface renderer)))
    ;; validate args plz...
    (object ([*globs* '()] [av-interface av])
      [(world? self) #t]
      [(->string self)
       (string-append "a wild world" " appears")]
      [(add-glob! self glob)
       (if (object? glob)
         [$ *globs* self (cons glob [$ *globs* self])])]
      ;; is it bad practice to have a method render
      ;; that just calls render on its component?
      [(render self) [$ draw-globs [$ av-interface self] [$ *globs* self]]]
      [(=? self other)
       (unless (world? other)
         (error 'world:=?
                "Don't know how compare world to non-world"
                self other))
       ;; compare fields here
       (= 1 1)])))

;; NOTE: move all this to a shapes.ss files or something.
;; NOTE add polygons, etc later if desired...
(define *known-shape-types* (make-parameter '(circle rectangle)))

(define new-point
  (let ([proto-point
          (object ()
            [(point? self) #t])])
    (lambda (x y)
      (when (not (and (integer? x) (integer? y) (>= x 0) (>= y 0)))
        (error "new-point" "invalid argument(s)" x y))
      (object ([x x] [y y])
        [(delegate self) proto-point]))))

(define-predicate point?)

(define new-colored-point
  (lambda (x y color)
    (when (not (color? color))
      (error "new-colored-point" "invalid color" color))
    (let ((proto-point (new-point x y)))
      (object ([color color])
        [(colored-point? self) #t]
        [(delegate self) proto-point]))))

(define-predicate colored-point?)

(define new-line
  (lambda (p1 p2)
    (when (not (and (point? p1) (point? p2)))
      (error "new-line" "invalid point(s)" p1 p2))
    (object ([start-point p1] [end-point p2])
      [(line? self) #t])))

(define-predicate line?)

(define new-colored-line
  (lambda (p1 p2 color)
    (let ((proto-line (new-line p1 p2)))
      (object ([color color])
        [(colored-line? self) #t]
        [(delegate self) proto-line]))))

(define-predicate colored-line?)

(define new-shape
  (let ([proto-shape
          (object ()
            [(shape? self) #t]
            ;[(draw self) (lambda (renderer) '())]
            ;; figure out how to make this something we "override"
            [(area self) -1])])
    (lambda (type)
      (when (not (member type (*known-shape-types*)))
        (error "my-shape" "unrecognized shape type" type))
      (object ([type type])
        [(delegate self) proto-shape]))))

(define-predicate shape?)

(define new-circle
  (let ([proto-shape (new-shape 'circle)]
        [*pi* 3.141592654])
    (lambda (pt r)
      (when (not (and (point? pt) (number? r)))
        (error "new-circle" "invalid argument(s)" pt r))
      (object ([point pt] [radius r])
        ;; "override" (e.g. shadow?) area in proto-shape
        [(circle? self) #t]
        [(area self) (* *pi* (expt [$ radius self] 2))]
        [(circumferance self) (* 2 *pi* [$ radius self])]
        [(delegate self) proto-shape]))))

(define-predicate circle?)

;; all these color objects
;; basically have the same shape.
;; it might be useful/interesting
;; to be able to add colors to
;; things without writing a whole
;; procedure to do so.
(define new-colored-circle
  (lambda (pt r color)
    (let ((proto-circ (new-circle pt r)))
      (when (not (color? color))
        (error "new-colored-circle" "invalid color" color))
      (object ([color color])
        [(colored-circle? self) #t]
        [(delegate self) proto-circ]))))

(define-predicate colored-circle?)

(define new-rectangle
  (let ([proto-shape (new-shape 'rectangle)])
    (lambda (top-left-point w h)
      (object ([origin top-left-point] [width w] [height h])
        [(rectangle? self) #t]
        [(area self) (* [$ width self] [$ height self])]
        [(perimiter self)
         (+ (* 2 [$ width self])
            (* 2 [$ height self]))]
        [(delegate self) proto-shape]))))

(define-predicate rectangle?)

(define new-colored-rectangle
  (lambda (origin w h color)
    (let ([proto-rect (new-rectangle origin w h)])
      (object ([color color])
        [(colored-rectangle? self) #t]
        [(delegate self) proto-rect]))))

(define-predicate colored-rectangle?)

;; actually "is" a rectangle, it just gets drawn differently.
(define new-cross
  (lambda (top-left-point w h)
    (let ((proto-rect (new-rectangle top-left-point w h)))
      (object ()
        [(cross? self) #t]
        [(get-lines self)
         (let* ((min-x [$ x [$ origin self]])
                (min-y [$ y [$ origin self]])
                (max-x (+ min-x [$ width self]))
                (max-y (+ min-y [$ height self])))
           (values (new-line (new-point min-x min-y)
                             (new-point max-x max-y))
                   (new-line (new-point max-x min-y)
                             (new-point min-x max-y))))]
        [(delegate self) proto-rect]))))

(define-predicate cross?)

(define new-colored-cross
  (lambda (top-left-point w h color)
    (let ((proto-cross (new-cross top-left-point w h)))
      (when (not (color? color))
        (error "new-colored-cross" "invalid color" color))
      (object ([color color])
        [(colored-cross? self) #t]
        [(delegate self) proto-cross]))))

(define-predicate colored-cross?)

;; e.g.
;(define c (new-circle (new-point 350 50) 33.1))
; dynamically add a method to point instances
;[$ add-method! [$ delegate [$ point c]] 'say-point (lambda (self) (display "i r pt")(newline))]

;(define (new-widget renderer dimensions)
  ;(object ([renderer renderer] [dimensions dimensions])))

;; (define w (new-world window))
;; [$ add-glob! w (object ((x -3)))]
;; [$ add-variable! w (field-spec foo 'bar])
;; [$ add-method! w 'poop (lambda (self) (display "takes poop")(newline)))]
