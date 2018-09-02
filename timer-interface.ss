(import (chezscheme)
        (sdl2)
        (tiny-talk))

;; we can get even more precise timers
;; by using sdl-get-performance-counter
(define new-one-shot-timer
  (lambda (ms fn)
    (let* ((birth-ticks (sdl-get-ticks))
           (id (string->symbol (gensym->unique-string (gensym)))))
      (object
        ([birth-ticks birth-ticks]
         [period-ms ms]
         [callback fn]
         [alive #t]
         [timer-id id])
        [(timer? self) #t]
        [(tick self)
         (let ((current-ticks (sdl-get-ticks)))
           (when (> (- current-ticks birth-ticks) [$ period-ms self])
             ([$ callback self])
             [$ alive self #f]))]))))

(define-predicate timer?)

;; maybe this approach is not the best...
;; even if it is, maybe we can use sdl-get-ticks
;; to create our own timers. that way we are only
;; using sdl for one thing. we can have the world
;; tick all of the timers once a frame, and then
;; they can take action if they have to
;; for now we will assume the function
;; takes two arguments and that returns an int
;; (hopefully valid values and 0 when appropriate)
;; can add an optional arg value to be passed to
;; callback when fired...
;(define new-timer
  ;(lambda (ms fn)
    ;(let ((sdl-timer-cb
            ;(foreign-callable __collect_safe fn (unsigned-32 void*) unsigned-32)))
      ;(lock-object sdl-timer-cb)
      ;(let ((entrypoint (foreign-callable-entry-point sdl-timer-cb))
            ;(timer-id -1)
            ;(is-destroyed #f))
        ;(object ([delay-ms ms] [entrypoint entrypoint] [timer-id -1])
          ;[(timer? self) #t]
          ;;; maybe arm should be locking the object
          ;;; and disarm should be unlocking it?
          ;[(arm self)
           ;(when [$ destroyed? self]
             ;(error "timer:arm" "attempted to arm when already destroyed"))
           ;(let ((new-timer-id (sdl-add-timer [$ delay-ms self] [$ entrypoint self] 0)))
             ;;; perhaps we should be keeping all of these instead...
             ;;; or we just have an armed flag that errors when we
             ;;; try and arm an already armed timer...
             ;[$ timer-id self new-timer-id])]
          ;[(disarm self)
           ;(when [$ destroyed? self]
             ;(error "timer:arm" "attempted to arm when already destroyed"))
           ;(when (not (= [$ timer-id self] -1))
             ;(sdl-remove-timer [$ timer-id self])
             ;[$ timer-id self -1])]
          ;[(destroyed? self) is-destroyed]
          ;[(destroy self)
           ;(when (not (= [$ timer-id self] -1))
             ;(sdl-remove-timer [$ timer-id self])
             ;(unlock-object sdl-timer-cb)
             ;(set! is-destroyed #t))])))))

;(define-predicate timer?)
