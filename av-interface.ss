
;; an attempt to try and hide the details of what
;; we have to make sdl do to present our cleanly-defined
;; objects
;; this will also make it easy to support other backends
;; in the future.
;; NOTE:
;; there are some things in the "main loop" that are exposed
;; sdl calls that would have to be addressed too.

(import (chezscheme)
        (sdl2)
        (tiny-talk))

(define new-av-interface
  ;; if we just passed in window,
  ;; world could create its own renderer...
  (lambda (window-renderer)
    (object ([renderer window-renderer])
      [(av-interface? self) #t]
      [(draw self thing)
       (cond ((shape? thing)
              (cond ((circle? thing) [$ draw-circle self thing])
                    ((rectangle? thing) [$ draw-rectangle self thing])
                    (else (error "av-interface:draw" "unrecognized shape" thing))))
             (else (error "av-interface:draw unrecognzied thing" thing)))]
      [(get-keyboard-input self) '()]
      ;; present a nice, clean mouse-input instance back instead of
      ;; gross sdl dealie...
      [(get-mouse-input self) '()]
      [(draw-pixel self point color)
       (let-values (((r g b a) [$ rgba color]))
         (sdl-set-render-draw-color [$ renderer self] r g b a)
         (sdl-render-draw-point [$ renderer self] [$ x point] [$ y point]))]
      [(draw-circle self circle)
       ()
       ]
      [(draw-globs self world)
       (map
         (lambda (glob)
           [$ draw self glob])
         [$ *globs* world])
       
       ;;(render self world)
       ;; maybe we go through world's *globs* and render them?
       ;; actually. i think we just keep a renderer that is of
       ;; a known size (full size of window-renderer (we'll only
       ;; support full-size worlds to start (for simplicity)))
       ;; and all the drawing is done to that.
       ;; this method then becomes a call to sdl-copy-renderer or whatever...

       ])))
