(import (chezscheme)
        (tiny-talk))

(define new-widget
  (lambda (bounding-rect border-color)
    (when (not (rectangle? bounding-rect))
      (error "new-widget" "invalid bounding-rect" bounding-rect))
    (when (not (color? border-color))
      (error "new-widget" "invalid border-color" border-color))
    (let ((has-focus #f)
          (children '()))
      (object ([bounding-rect bounding-rect] [border-color border-color])
        [(widget? self) #t]
        [(border self)
         (new-colored-rectangle
           [$ origin [$ bounding-rect self]]
           [$ width [$ bounding-rect self]]
           [$ height [$ bounding-rect self]]
           [$ border-color self])]
        ;; sort of "implements" event-handler
        ;; this seems bad, but if we want
        ;; multiple proto objects, do we just
        ;; write our own methods that just
        ;; pass received arguments to the methods
        ;; of the same name on the proto objects?
        ;; or is it ok to just "pretend" to be an object
        ;; or a different kind? I wonder what they
        ;; call this if anything?
        [(event-handler? self) #t]
        [(handle-event self e)
         (when (not (event? e))
           (error "widget:handle-event" "invalid event received" e))
         ;; when we add other types this will obviously
         ;; be a cond...
         (when (mouse-event? e)
           (when (eq? [$ mouse-event-type e] 'single-click)
             (display "the widget has been single clicked!")
             (newline)))]
        [(children self) children]
        [(add-child! self w)
         (when (not (widget? w))
           (error "widget:add-child!" "invliad child received" w))
         (set! children (cons w children))]
        [(has-focus self) has-focus]))))

;; maybe we should have widget, and atomic-widget
;; where AW would be things that aren't allowed to
;; have children. like a text widget. its just
;; a clickable rectangle with some text written in it.
(define new-text-widget
  '())

(define-predicate widget?)
