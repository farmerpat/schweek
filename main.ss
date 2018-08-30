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

(import (chezscheme)
        (sdl2)
        (sdl2 ttf)
        (tiny-talk))

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

(define (sdl-common-event-t? ob)
  (and (ftype-pointer? ob)
       (ftype-pointer? sdl-common-event-t ob)))

(define (sdl-event-t? ob)
  (and (ftype-pointer? ob)
       (ftype-pointer? sdl-event-t ob)))

(define (get-type event)
  (if (ftype-pointer? event)
      (ftype-ref sdl-event-t (type) event)))

(define (get-type-symbol event)
  (if (ftype-pointer? event)
      (sdl-event-type-ref (get-type event))))

(define (get-mouse-event-button mouse)
  (if (ftype-pointer? mouse)
    (ftype-ref sdl-mouse-button-event-t (button) mouse)))

(define (get-mouse-event-x mouse)
  (if (ftype-pointer? mouse)
    (ftype-ref sdl-mouse-button-event-t (x) mouse)))

(define (get-mouse-event-y mouse)
  (if (ftype-pointer? mouse)
    (ftype-ref sdl-mouse-button-event-t (y) mouse)))

(define (is-window-event? event)
  (eq? 'windowevent (get-type-symbol event)))

(define (is-quit-event? event)
  (eq? 'quit (get-type-symbol event)))

(define (is-key-down-event? event)
  (eq? 'keydown (get-type-symbol event)))

(define (is-key-up-event? event)
  (eq? 'keyup (get-type-symbol event)))

(define (is-mouse-buttom-down-event? event)
  (eq? 'mousebuttondown (get-type-symbol event)))

;; does this become get-event-kbd-event ?
;; it think it does.
;; as mentioned above, figure out how to
;; auto-generate this stuff as soon as possible.
(define (get-kbd-event event)
  (ftype-&ref sdl-event-t (key) event))

(define (get-mouse-event event)
  (ftype-&ref sdl-event-t (button) event))

;; same as type in parent sdl-event-t
(define (get-kbd-event-type kbd-event-ptr)
  (ftype-ref sdl-keyboard-event-t (type) kbd-event-ptr))

;; sdl-keysym-t
(define (get-kbd-event-keysym kbd-event-ptr)
  (ftype-&ref sdl-keyboard-event-t (keysym) kbd-event-ptr))

(define (get-keysym-key-code ksm)
  (ftype-&ref sdl-keysym-t (sym) ksm))

(define (get-keysym-scancode ksm)
  (ftype-&ref sdl-keysym-t (scancode) ksm))

(define gen-rect
  (case-lambda
    ((gw gh) (gen-rect 0 0 gw gh))
    ((gx gy gw gh)
     (new-struct sdl-rect-t (x gx) (y gy) (w gw) (h gh)))))

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

(define (generate-kbd-code-predicate sym)
  (lambda (kbd-event)
    (let* ((ksym (get-kbd-event-keysym kbd-event))
           (key-code-ptr (get-keysym-key-code ksym))
           (code (ftype-ref int () key-code-ptr)))
      (if (eq? (sdl-keycode sym) code) #t #f))))

(define (generate-keydown-handler pred? callback)
  (lambda (event)
    (when (is-key-down-event? event)
      (let ((kbe (get-kbd-event event)))
       (if (ftype-pointer? kbe)
           (if (pred? kbe)
               (callback)))))))

(define (generate-mouse-down-hanlder pred? callback)
  (lambda (event)
    (when (is-mouse-buttom-down-event? event)
      (let ((mouse (get-mouse-event event)))
        (if (ftype-pointer? mouse)
          (if (pred? mouse)
            (callback)))))))

(define (clear-renderer)
  (sdl-set-render-draw-color
    renderer
    (bg-color-r)
    (bg-color-g)
    (bg-color-b)
    (bg-color-a))
  (sdl-render-fill-rect renderer (gen-rect 1024 768)))

(sdl-library-init)
;(sdl-ttf-library-init)

;(sdl-shim-ttf-init "/home/patrick/scheme-libs/thunderchez/sdl2/ttf-shim/ttfshim.so")
(load-shared-object "/home/patrick/scheme-libs/thunderchez/sdl2/ttf-shim/ttfshim.so")
(ttf-init)

(define retval  (sdl-init (sdl-initialization 'video 'events)))

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
     (new-colored-font "DejaVuSerif.ttf" 'deja-vu-serif 26 color-black)
     (new-point 500 500))]

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

(register-event-handler
  (lambda (event)
    (when (is-mouse-buttom-down-event? event)
      (let ((mouse (get-mouse-event event)))
        (if (ftype-pointer? mouse)
          ;; grab some shit...
          (let ((button (get-mouse-event-button mouse))
                (x (get-mouse-event-x mouse))
                (y (get-mouse-event-y mouse)))
            (display x)
            (display ", ")
            (display y)
            (newline)))))))

;; now register a mouse single click event handler...
;; and we can start doing things at its location (open a menu, for instance...)
;; this basically is the chez branch of schengine.
;; this is really a paradigm for an app and not just a game, where a game is an app.
;; keep your eyes peeled for a nice abstraction or two...

(let mortality-loop ()
 (sdl-poll-event event)
 (pass-event-to-event-handlers event)
 (clear-renderer)
 ;; draw globs
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
