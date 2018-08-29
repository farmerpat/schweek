;; if we hack add-method! etc, it may become possible to save
;; world state via serialization. we would have to save
;; the added lambdas making up the added serializable methods.

;; i sort of see each world as a collection of widgets/objects
;; and a scheme environment. maybe there is some way to
;; monkey with all that w/o hacking the scheme...

;; we have (at least)...
;environment
;copy-environment
;environment
;environment-mutable?
;environment-symbols
;environment?
;ieee-environment
;interaction-environment
;null-environment
;scheme-environment
;scheme-report-environment
; im not sure that we can do
; this with environments. maybe
; *globs* will be enough.

;; can't we load this conditionally?
(load "tiny-talk.sls")

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
      [(render self) [$ draw-globs [$ av-interface self] self]]
      [(=? self other)
       (unless (world? other)
         (error 'world:=?
                "Don't know how compare world to non-world"
                self other))
       ;; compare fields here
       (= 1 1)])))

;; eff polygons for now. circs and rects only.
;; also lines....
(define *known-shape-types* (make-parameter '(circle rectangle)))

(define make-point
  (let ([proto-point
          (object ()
            [(point? self) #t])])
    (lambda (x y)
      (when (not (and (number? x) (number? y)))
        (error "make-point" "invalid argument(s)" x y))
      (object ([x x] [y y])
        [(delegate self) proto-point]))))

(define-predicate point?)

(define make-shape
  (let ([proto-shape
          (object ()
            [(shape? self) #t]
            [(draw self) (lambda (renderer) '())]
            ;; figure out how to make this something we "override"
            [(area self) -1])])
    (lambda (type)
      (when (not (member type (*known-shape-types*)))
        (error "my-shape" "unrecognized shape type" type))
      (object ([type type])
        [(delegate self) proto-shape]))))

(define-predicate shape?)

(define make-circle
  (let ([proto-shape (make-shape 'circle)]
        [*pi* 3.141592654])
    (lambda (pt r)
      (when (not (and (point? pt) (number? r)))
        (error "make-circle" "invalid argument(s)" pt r))
      (object ([point pt] [radius r])
        ;; "override" (e.g. shadow?) area in proto-shape
        [(circle? self) #t]
        [(area self) (* *pi* (expt [$ radius self] 2))]
        [(circumferance self) (* 2 *pi* [$ radius self])]
        [(delegate self) proto-shape]))))

(define-predicate circle?)

(define make-rectangle
  (let ([proto-shape (make-shape 'rectangle)])
    (lambda (top-left-point w h)
      (object ([origin top-left-point] [width w] [height h])
        [(rectangle? self) #t]
        [(area self) (* [$ width self] [$ height self])]
        [(perimiter self)
         (+ (* 2 [$ width self])
            (* 2 [$ height self]))]
        [(delegate self) proto-shape]))))

(define-predicate rectangle?)

;; e.g.
;(define c (make-circle (make-point 350 50) 33.1))
; dynamically add a method to point instances
;[$ add-method! [$ delegate [$ point c]] 'say-point (lambda (self) (display "i r pt")(newline))]

;(define (new-widget renderer dimensions)
  ;(object ([renderer renderer] [dimensions dimensions])))

;; (define w (new-world window))
;; [$ add-glob! w (object ((x -3)))]
;; [$ add-variable! w (field-spec foo 'bar])
;; [$ add-method! w 'poop (lambda (self) (display "takes poop")(newline)))]
