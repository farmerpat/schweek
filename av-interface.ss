
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

(define new-av-interface
  ;; if we just passed in window,
  ;; world could create its own renderer...
  (lambda (window-renderer)
    (object ([renderer window-renderer])
      [(av-interface? self) #t]
      [(draw self thing)
       (cond ((colored-point? thing)
              [$ draw-colored-pixel self thing])
             ((colored-line? thing)
              [$ draw-line self thing])
             ((shape? thing)
              (cond ((circle? thing)
                     (if (colored-circle? thing)
                       [$ draw-colored-circle self thing]
                       [$ draw-circle self thing]))
                    ((rectangle? thing)
                     (if (colored-rectangle? thing)
                       [$ draw-colored-rectangle self thing]
                       [$ draw-rectangle self thing]))
                    (else (error "av-interface:draw" "unrecognized shape" thing))))
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
      [(draw-globs self globs)
       (map
         (lambda (glob)
           [$ draw self glob])
         globs)])))
