schwe[ae]k
----------

- inspired by seeing/poking at a squeak smalltalk vm
- related to schengine sort of..

- i think it makes sense to abstract away even more
  SDL and use the png hacking i have done elsewhere
  to create a representation of a surface that will
  get transformed into an sdl (or whatever) surface
  that becomes the entire window contents (or part
  of it I guess. It could be as flexible as we like)
  This abstractionwill get SDL further away from more
  places in the code (e.g. drawing shapes and fonts)
  This would leave me to devise another way to render
  fonts. perhaps I can just create/pull some crude bitmap
  in one point to use as a place-holder.

- it is interesting to think that I will be able
  to test visual output a single frame at a time
  by rendering to a ppm instead
  this would actually make unit-testing drawing
  functions possible, which seems like a good
  thing.

TODO
----
- expand event-interface.ss
- create sound-interface.ss
