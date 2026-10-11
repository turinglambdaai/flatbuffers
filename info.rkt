#lang info

(define collection 'multi)
(define name "flatbuffers")
(define version "0.1.0")
(define pkg-desc
  "Pure-Racket implementation of the FlatBuffers wire format: a generic
zero-copy reader and a low-level builder (32-bit offsets, little-endian).
No schema codegen, no FFI, no dependencies beyond `base`.")
(define pkg-authors '("turinglambdaai"))
(define license 'MIT)
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/flatbuffers.scrbl" ())))
;; `raco test -p` should run only the test suite, not the .scrbl module.
(define test-omit-paths '("flatbuffers/scribblings/flatbuffers.scrbl"))
