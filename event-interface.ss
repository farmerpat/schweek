(import (chezscheme)
        ;(sdl2)
        ;(sdl2 ttf)
        (tiny-talk))

;; where keypress is triggered by
;; a complete (down/up) key press
(define *event-types*
  (make-parameter '(mouse keypress)))

(define *mouse-event-types*
  (make-parameter '(single-click double-click)))

(define new-event
  (lambda (type)
    (when (not (member type (*event-types*)))
      (error "new-event" "unrecognized event type" type))
    (object ([type type])
      [(event? self) #t])))

(define-predicate event?)

(define new-mouse-event
  (lambda (mouse-event-type p)
    (when (not (member mouse-event-type (*mouse-event-types*)))
      (error "new-mouse-event" "unrecognized mouse event type" mouse-event-type))
    (when (not (point? p))
      (error "new-mouse-event" "unrecognized point" p))
    (let ((proto-event (new-event 'mouse)))
      (object ([mouse-event-type mouse-event-type] [coords p])
        [(mouse-event? self) #t]
        [(delegate self) proto-event]))))

(define-predicate mouse-event?)

;; creating single-click-event-handler, etc
;; is probably a good idea...
(define new-event-handler
  (lambda (handler)
    (object ([handler handler])
      [(event-handler? self) #t]
      [(handle-event self e)
       (when (not (procedure? [$ handler self]))
         (error "event-handler:process-event" "invalid handler" [$ handler self]))
       (when (not (event? e))
         (error "event-handler:process-event" "invalid event received" e))
       ([$ handler self] e)])))

(define-predicate event-handler?)
