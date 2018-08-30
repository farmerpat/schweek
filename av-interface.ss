
;; an attempt to try and hide the details of what
;; we have to make sdl do to present our cleanly-defined
;; objects
;; this will also make it easy to support other backends
;; in the future.
;; NOTE:
;; there are some things in the "main loop" that are exposed
;; sdl calls that would have to be addressed too.

;; probably just put tiny-talk in ~/scheme-libs
;(load "color.ss")
(import (chezscheme)
        (sdl2)
        (sdl2 ttf)
        (tiny-talk))

;; put me in a utils file or something...
;; ...better yet make a my-utils library,
;; track it, and stick it in ~/scheme-libs
(define-syntax while
  (lambda (stx)
    (syntax-case stx ()
      [(_ pred sexp1 sexp2 ...)
       (syntax
         (let while-loop ()
           sexp1
           sexp2
           ...
           (if pred (while-loop))))])))

;; in general, need to thinking
;; about how we are going to deal
;; with "resources" in the system
;; that will become part of the world glob list
(define new-font
  (lambda (file name point)
    ;; plz check first...
    (let ((sdl-font (ttf-open-font file point)))
      (object ([name name] [file file] [point point] [sdl-font sdl-font])
        [(font? self) #t]
        ;; it would be nice if we could use define-ftype-allocator
        ;; or something that will free this automatically once
        ;; the object containing it goes out of scope...
        [(free-resources! self)
         ;; should probably validate this thing first...
         (ttf-close-font sdl-font)]))))

(define-predicate font?)

(define new-colored-font
  (lambda (file name point color)
    (let ((proto-font (new-font file name point)))
      (object ([color color])
        [(colored-font? self) #t]
        [(delegate self) proto-font]))))

(define-predicate colored-font?)

;; font-color border-color?
;; create bordered-colored-lable
(define new-label
  (lambda (text font rect)
    (when (or (not (string? text))
              (not (font? font))
              (not (rectangle? rect)))
      (error "new-label" "invalid arg(s)" text font))
    (object ([text text] [font font] [bounding-rect rect])
      [(label? self) #t])))

(define-predicate label?)

(define new-colored-label
  (lambda (text font rect color)
    (when (not (color? color))
      (error "new-colored-label" "invalid color" color))
    (let ((proto-label (new-label text font rect)))
      (object ([color color])
        [(colored-label? self) #t]
        [(delegate self) proto-label]))))

(define-predicate colored-label?)

(define-ftype-allocator uint8-guarded-ptr uint8)

;; this is getting huge.
;; to start spit off font stuff
;; into av-font-interface
;; points/lines could be another
;; object its composed of.
;; as could a specific shape interface
(define new-av-interface
  ;; if we just passed in window,
  ;; world could create its own renderer...
  (lambda (window-renderer)
    (object ([renderer window-renderer])
      [(av-interface? self) #t]
      [(draw self thing)
       (cond ((point? thing)
              (if (colored-point? thing)
                [$ draw-colored-pixel self thing]
                [$ draw-pixel self thing]))
             ((line? thing)
              (if (colored-line? thing)
                [$ draw-colored-line self thing]
                [$ draw-line self thing]))
             ((shape? thing)
              (cond ((circle? thing)
                     (if (colored-circle? thing)
                       [$ draw-colored-circle self thing]
                       [$ draw-circle self thing]))
                    ;; as it stands, we have to check
                    ;;cross before rectangle, or make
                    ;; the if in rectangle? clause
                    ;; a cond that checks for rect
                    ;; and colored rect.
                    ;; b/c a cross "is" a rect.
                    ((cross? thing)
                     (if (colored-cross? thing)
                       [$ draw-colored-cross self thing]
                       [$ draw-cross self thing]))
                    ((rectangle? thing)
                     (if (colored-rectangle? thing)
                       [$ draw-colored-rectangle self thing]
                       [$ draw-rectangle self thing]))
                    (else (error "av-interface:draw" "unrecognized shape" thing))))
             ((label? thing)
              (if (colored-label? thing)
                [$ draw-colored-label self thing]
                [$ draw-label self thing]))
             (else (error "av-interface:draw unrecognzied thing" thing)))]
      [(get-keyboard-input self) '()]
      ;; present a nice, clean mouse-input instance back instead of
      ;; gross sdl dealie...
      [(get-mouse-input self) '()]
      [(draw-pixel self point)
       (sdl-render-draw-point [$ renderer self] [$ x point] [$ y point])]
      [(draw-colored-pixel self point)
       (let-values (((r g b a) [$ rgba [$ color point]]))
         ;; TODO: should be saving and restoring current draw-color
         (sdl-set-render-draw-color [$ renderer self] r g b a)
         [$ draw-pixel self point])]
      ;; TODO: add draw-lines, draw-colored-lines, etc.
      [(draw-line self line)
       (sdl-render-draw-line
         [$ renderer self]
         [$ x [$ start-point line]]
         [$ y [$ start-point line]]
         [$ x [$ end-point line]]
         [$ y [$ end-point line]])]
      [(draw-colored-line self line)
       (let-values (((r g b a) [$ rgba [$ color line]]))
         ;; TODO: should be saving and restoring current draw-color
         (sdl-set-render-draw-color [$ renderer self] r g b a)
         [$ draw-line self line])]
      ;; this could be on rectangle object,
      ;; but that would infect other areas
      ;; with sdl-specific code...
      [(rectangle->sdl-rect self rect)
       (let ((rx [$ x [$ origin rect]])
             (ry [$ y [$ origin rect]])
             (rw [$ width rect])
             (rh [$ height rect]))
         (new-struct sdl-rect-t (x rx) (y ry) (w rw) (h rh)))]
      [(draw-filled-rectangle self rect)
       (let ((sdl-rect [$ rectangle->sdl-rect self rect]))
         (sdl-render-fill-rect [$ renderer self] sdl-rect))]
      [(draw-rectangle self rect)
       (let ((sdl-rect [$ rectangle->sdl-rect self rect]))
         (sdl-render-draw-rect [$ renderer self] sdl-rect))]
      [(draw-colored-rectangle self rect)
       ;; set the color
       ;; this let-values, set-color peice should be
       ;; abstracted away...
       (let-values (((r g b a) [$ rgba [$ color rect]]))
         ;; TODO: should be saving and restoring current draw-color
         (sdl-set-render-draw-color [$ renderer self] r g b a)
         [$ draw-rectangle self rect])]
      [(draw-colored-cross self cross)
       (let-values (((r g b a) [$ rgba [$ color cross]]))
         (sdl-set-render-draw-color [$ renderer self] r g b a)
         [$ draw-cross self cross])]
      [(draw-cross self cross)
       (let-values (((l1 l2) [$ get-lines cross]))
         [$ draw-line self l1]
         [$ draw-line self l2])]
      [(draw-colored-circle self circle)
       (let-values (((r g b a) [$ rgba [$ color circle]]))
         ;; TODO: there should be a method to set render color
         ;; that just takes a color ob. that will remove
         ;; a lot of this duplicated stuff.
         (sdl-set-render-draw-color [$ renderer self] r g b a)
         [$ draw-circle self circle])]
      [(draw-circle self circle)
       ;; implementation of the midpoint circle algorithm
       ;; https://en.wikipedia.org/wiki/Midpoint_circle_algorithm
       ;;(draw-colored-pixel self point)
       (let* ((radius (inexact->exact [$ radius circle]))
              (x0 [$ x [$ point circle]])
              (y0 [$ y [$ point circle]])
              (x (sub1 radius))
              (y 0) (dx 1) (dy 1)
              (err (- dx (bitwise-arithmetic-shift-left radius 1))))
         (while (>= x y)
           [$ draw-pixel self (new-point (+ x0 x) (+ y0 y))]
           [$ draw-pixel self (new-point (+ x0 y) (+ y0 x))]
           [$ draw-pixel self (new-point (- x0 y) (+ y0 x))]
           [$ draw-pixel self (new-point (- x0 x) (+ y0 y))]
           [$ draw-pixel self (new-point (- x0 x) (- y0 y))]
           [$ draw-pixel self (new-point (- x0 y) (- y0 x))]
           [$ draw-pixel self (new-point (+ x0 y) (- y0 x))]
           [$ draw-pixel self (new-point (+ x0 x) (- y0 y))]

           (when (<= err 0)
             (set! y (add1 y))
             (set! err (+ err dy))
             (set! dy (+ dy 2)))

           (when (> err 0)
             (set! x (sub1 x))
             (set! dx (+ dx 2))
             (set! err (+ err (- dx (bitwise-arithmetic-shift-left radius 1)))))))]
      [(draw-colored-label self label)
       [$ set-render-draw-color! self [$ color label]]
       [$ draw-label self label]]
      [(draw-label self label)
       [$ draw-filled-rectangle self [$ bounding-rect label]]

       (when (colored-font? [$ font label])
         [$ set-render-draw-color! self [$ color [$ font label]]])

       ;; garbage-collected automatically?
       (let* ((sdl-color [$ get-render-draw-color self])
              (surface (sttf-render-text-solid
                         [$ sdl-font [$ font label]]
                         [$ text label]
                         sdl-color))
              (ideal-width (ftype-ref sdl-surface-t (w) surface))
              (ideal-height (ftype-ref sdl-surface-t (h) surface))
              (texture (sdl-create-texture-from-surface [$ renderer self] surface))
              (og-origin [$ origin [$ bounding-rect label]])
              (*padding* 2)
              (point (new-point (+ *padding* [$ x og-origin])
                                (+ *padding* [$ y og-origin])))
              (dest-rect (new-rectangle point ideal-width ideal-height)))

         (sdl-render-copy
           [$ renderer self]
           texture
           (make-ftype-pointer sdl-rect-t 0)
           ;(make-ftype-pointer sdl-rect-t 0)
           ;; could (optionally?) shrink for padding...
           [$ rectangle->sdl-rect self dest-rect]
           ;[$ rectangle->sdl-rect self [$ gen-inner-padded-rect [$ bounding-rect label] 0]]
           )
         )
       ]
      ;; TODO: use me all over
      [(set-render-draw-color! self color)
       (let-values (((r g b a) [$ rgba color]))
         (sdl-set-render-draw-color [$ renderer self] r g b a))]
      [(get-render-draw-color self)
       (let ((r-ptr (uint8-guarded-ptr))
             (g-ptr (uint8-guarded-ptr))
             (b-ptr (uint8-guarded-ptr))
             (a-ptr (uint8-guarded-ptr)))
         (when (negative? (sdl-get-render-draw-color
                            [$ renderer self]
                            r-ptr g-ptr b-ptr a-ptr))
           (error "av-interface:get-render-draw-color" "sdl-get-render-draw-color failed"))
         (new-struct sdl-color-t
           (r (ftype-ref uint8 () r-ptr))
           (g (ftype-ref uint8 () g-ptr))
           (b (ftype-ref uint8 () b-ptr))
           (a (ftype-ref uint8 () a-ptr))))]
      [(draw-globs self globs)
       (map
         (lambda (glob)
           [$ draw self glob])
         globs)])))
