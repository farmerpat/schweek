;; NOTE:
;;   adding image magick bindings
;;   would be super cool. could have
;;   image objects that allow IM
;;   transformations/conversion to be performed on them

;; in addition to av-interface, maybe we also have
;; an event-interface. It would be really nice
;; to be able to add event listeners like
;; 'mouse-single-click and 'mouse-double-click
;; instead of the client of the event registrar
;; having to deal with mouse down events firing on
;; a million frames. of course, we will also add
;; events for 'mouse-down, etc for more fine-grained
;; control.
(load "tiny-talk.sls")
(load "world.ss")
(load "color.ss")
(load "av-interface.ss")
(load "timer-interface.ss")
(load "event-interface.ss")

(import (chezscheme)
        (sdl2)
        (sdl2 ttf)
        (tiny-talk))

(load "sdl-helpers.ss")

(define *quit* (make-parameter #f))

(define *bg-color* (make-parameter (list 80 80 80 255)))

(define (bg-color-r)
  (car (*bg-color*)))

(define (bg-color-g)
  (cadr (*bg-color*)))

(define (bg-color-b)
  (caddr (*bg-color*)))

(define (bg-color-a)
  (cadddr (*bg-color*)))

(define *event-handlers* (make-parameter '()))

(define (register-event-handler eh)
  (*event-handlers* (cons eh (*event-handlers*))))

;; i am repeating myself with
;; these parameters whose values
;; are lists of procedures
;; that we want to call w/ or w/o
;; arguments. abstract away...
(define *draw-procedures* (make-parameter '()))

(define (register-draw-procedure dp)
  (*draw-procedures* (cons dp (*draw-procedures*))))

(define (call-all lst)
  (cond ((null? lst) '())
        ((procedure? (car lst))
         ((car lst))
         (call-all (cdr lst)))))

(define (call-all-with-arg lst arg)
  (cond ((null? lst) '())
        ((procedure? (car lst))
         ((car lst) arg)
         (call-all-with-arg (cdr lst) arg))))

(define (pass-event-to-event-handlers event)
  (call-all-with-arg (*event-handlers*) event))

(define (call-draw-procedures)
  (call-all (*draw-procedures*)))

(sdl-library-init)
;(sdl-ttf-library-init)

;(sdl-shim-ttf-init "/home/patrick/scheme-libs/thunderchez/sdl2/ttf-shim/ttfshim.so")
(load-shared-object "/home/patrick/scheme-libs/thunderchez/sdl2/ttf-shim/ttfshim.so")
(ttf-init)

;; for some reason the first call always returns 0,
;; which screws up our timers...
(display (sdl-get-ticks))
(newline)

(define retval  (sdl-init (sdl-initialization 'video 'events 'timer 'audio)))

(define window (sdl-create-window "test" 50 50 800 600 (sdl-window-flags 'shown)))

(define renderer (sdl-create-renderer window -1 (sdl-renderer-flags 'accelerated)))

(define w (new-world renderer))
(define color-black [$ get-color *c64-palette* 'black])
(define color-white [$ get-color *c64-palette* 'white])

;; make some points and add them to the world
(do ((i 0  (+ i 1)))
  ((> i 400) ' ())
  [$ add-glob! w (new-colored-point i i color-black)])

[$ add-glob! w (new-colored-line (new-point 0 200) (new-point 400 200) color-black)]

[$ add-glob! w (new-colored-rectangle (new-point 300 400) 50 50 color-black)]

(do ((i 0 (+ i 50)))
  ((> i 400) '())
  [$ add-glob! w (new-colored-circle (new-point (+ i 200) 200) 25 color-white)])

[$ add-glob! w (new-colored-cross (new-point 500 100) 10 10 color-black)]

;; should see if it's possible to calculate
;; what the dimensions of the label should be
;; based on how much space the message in
;; font/point will take up.

[$ add-glob!
   w
   (new-label
     "message!"
     (new-colored-font "DejaVuSerif.ttf" 'deja-vu-serif 56 color-black)
     (new-point 500 400))]

[$ add-glob!
   w
   (new-bordered-label
     "it was the best of times, it was the worst of times..."
     (new-colored-font "DejaVuSans.ttf" 'deja-vu-sans 26 color-black)
     (new-point 100 500)
     (new-border [$ get-color *c64-palette* 'light-red] 15))]

[$ add-glob! w
   (new-one-shot-timer 3000 (lambda () (display "fyf alot") (newline)))]

[$ add-glob! w
   (new-event-handler
     (lambda (e)
       (if (mouse-event? e)
         (begin
           (display "das mouse ")
           (display "at: ")
           (display [$ ->string [$ coords e]])
           (newline)))))]

(define src-rect
  (gen-rect 64 64))

(define dest-rect
  (gen-rect 200 200 64 64))

(define event
  (new-struct sdl-event-t))

;; should create syntax that will
;; create a sprite and register it
;; to be drawn.
;; perhaps the draw procedure is kept
;; as a property of the "sprite" in
;; a closure
;(register-draw-procedure
  ;(lambda ()
    ;(sdl-render-copy renderer img-texture src-rect dest-rect)))

(register-event-handler
  (generate-keydown-handler
    (generate-kbd-code-predicate 'escape)
    (lambda () (*quit* #t))))

;(define *click-initiated-flag* #f)
;(define *single-click-available-flag* #t)

(define *atomic-click-initiated-flag*
  (new-struct sdl-atomic-t))

(sdl-atomic-set *atomic-click-initiated-flag* 0)

(define *atomic-single-click-available-flag*
  (new-struct sdl-atomic-t))

(sdl-atomic-set *atomic-single-click-available-flag* 1)

(register-event-handler
  (lambda (event)
    (if (is-mouse-button-up-event? event)
      (if (not (zero? (sdl-atomic-get *atomic-click-initiated-flag*)))
        (begin
          (if (not (zero? (sdl-atomic-get *atomic-single-click-available-flag*)))
            (let* ((mouse (get-mouse-event event))
                   (x (get-mouse-event-x mouse))
                   (y (get-mouse-event-y mouse))
                   (e (new-mouse-event 'single-click (new-point x y))))
              [$ process-event w e]
              (sdl-atomic-set *atomic-click-initiated-flag* 0))
            (begin
              (sdl-atomic-set *atomic-click-initiated-flag* 0)
              (sdl-atomic-set *atomic-single-click-available-flag* 1)))))
      (if (is-mouse-button-down-event? event)
        (if (zero? (sdl-atomic-get *atomic-click-initiated-flag*))
          (begin
            (sdl-atomic-set *atomic-click-initiated-flag* 1)
            (sdl-atomic-set *atomic-single-click-available-flag* 1)
            [$ add-glob! w
               (new-one-shot-timer
                 500
                 (lambda ()
                   (sdl-atomic-set *atomic-single-click-available-flag* 0)))]))))))

;; now register a mouse single click event handler...
;; and we can start doing things at its location (open a menu, for instance...)
;; this basically is the chez branch of schengine.
;; this is really a paradigm for an app and not just a game, where a game is an app.
;; keep your eyes peeled for a nice abstraction or two...

;; need
;;  window-interface.ss
;;  and to clean up interface abstraction lines

(let mortality-loop ()
  ;; maybe we should be flushing these...
  (while (not (zero? (sdl-poll-event event)))
    (pass-event-to-event-handlers event))

  (clear-renderer)
  [$ tick w]
  [$ render w]
  (sdl-render-present renderer)

 (if (not (*quit*))
   (mortality-loop)))

(sdl-destroy-window window)
;(sdl-destroy-texture img-texture)
(sdl-destroy-renderer renderer)

(ttf-quit)
(sdl-quit)

(scheme-start (lambda fns '()))
