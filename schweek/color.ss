(library (schweek color)
  (export
    new-color
    color?
    new-named-color
    named-color?
    new-palette
    palette?
    *c64-palette*)
  (import (chezscheme)
          ;(sdl2) ; will need when converting to sdl color.
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
           [$ a self])])))

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

  (define new-palette
    (lambda (name colors)
      (object ([name name] [colors colors])
        [(palette? self) #t]
        [(add-color! self color)
         (when (not (color? color))
           (error "new-pallet:add-color!" "not a color" color))
         [$ colors self (cons color [$ colors self])]]
        [(get-color self name)
         (first-that
           (lambda (c) (and (named-color? c) (eq? name [$ name c])))
           [$ colors self])]
        ;; add methods to produce a random color, etc
        )))

  (define-predicate palette?)

  (define *c64-palette*
    (new-palette
      'c64
      (list (new-named-color 0 0 0 255 'black)
            (new-named-color 255 255 255 255 'white)
            (new-named-color 136 0 0 255 'red)
            (new-named-color 170 255 238 255 'cyan)
            (new-named-color 204 64 205 255 'violet)
            (new-named-color 0 204 85 255 'green)
            (new-named-color 0 0 170 255 'blue)
            (new-named-color 238 238 119 255 'yellow)
            (new-named-color 221 136 85 255 'orange)
            (new-named-color 102 68 0 255 'brown)
            (new-named-color 255 119 119 255 'light-red)
            (new-named-color 51 51 51 255 'grey1)
            (new-named-color 119 119 119 255 'grey2)
            (new-named-color 187 187 187 255 'gre3)
            (new-named-color 170 255 102 255 'light-green)
            (new-named-color 0 136 255 255 'light-blue)))))
