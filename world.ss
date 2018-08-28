;; if we hack add-method! etc, it may become possible to save
;; world state via serialization. we would have to save
;; the added lambdas making up the added serializable methods.

(import (chezscheme)
        (sdl2)
        (tiny-talk))

(define (new-world window)
  ;; validate args plz...
  (object ([*globs* '()] [window window])
    [(world? self) #t]
    [(->string self)
     (string-append "a wild world" " appears")]
    [(add-glob! self glob)
     [$ *globs* self (cons glob [$ *globs* self])]]
    [(=? self other)
     (unless (world? other)
       (error 'world:=?
              "Don't know how compare world to non-world"
              self other))
     ;; compare fields here
     (= 1 1)]))

;; (define w (new-world window))
;; [$ add-glob! w (object ((x -3)))]
;; [$ add-variable! w (field-spec foo 'bar])
;; [$ add-method! w 'poop (lambda (self) (display "takes poop")(newline)))]
