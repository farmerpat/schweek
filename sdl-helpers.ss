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

(define (is-mouse-button-down-event? event)
  (eq? 'mousebuttondown (get-type-symbol event)))

(define (is-mouse-button-up-event? event)
  (eq? 'mousebuttonup (get-type-symbol event)))

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
    (when (is-mouse-button-down-event? event)
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
