(import (chezscheme)
        (sdl2)
        (tiny-talk))

(define new-color
  (lambda (r g b a)
    (object ([name #f] [r r] [g g] [b b] [a a])
      [(color? self) #t]
      [(rgba self)
       (values
         [$ r self]
         [$ g self]
         [$ b self]
         [$ a self])]
      [(delegate self) proto-ob])))

(define new-named-color
  (lambda (r g b a name)
    (let ((proto-color (new-color r g b a)))
      (object ([name name])
        [(named-color? self) #t]
        [(delegate self) proto-color]))))

(define-predicate color?)
(define-predicate named-color?)

(define (first-that pred? lst)
  (if (null? lst)
    #f
    (if (pred? (car lst))
      (car lst)
      (first-that pred? (cdr lst)))))

(define new-pallette
  (lambda (name colors)
    (object ([name name] [colors colors])
      [(palette? self) #t]
      [(add-color! self color)
       (when (not? (color? color))
         (error "new-pallet:add-color!" "not a color" color))
       [$ colors self (cons color [$ colors self])]]
      [(get-color-named self name)
       (first-that
         (lambda (c) (and (named-color? c) (eq? name [$ name c])))
         [$ colors self])]
      ;; add methods to produce a random color, etc
      )
    )
  )
