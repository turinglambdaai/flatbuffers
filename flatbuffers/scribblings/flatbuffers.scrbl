#lang scribble/manual

@(require (for-label racket/base flatbuffers)
          scribble/example)

@(define shared-eval
   (make-base-eval #:lang 'racket/base '(require flatbuffers)))

@title{FlatBuffers: Generic Reader and Builder}
@author[@(tt "turinglambdaai")]

@defmodule[flatbuffers]

This package is a pure-Racket implementation of the
@hyperlink["https://flatbuffers.dev/" ]{FlatBuffers} wire format — Google's
zero-copy serialization format.  It provides:

@itemlist[
 @item{a @emph{generic reader} that decodes any FlatBuffers buffer by
   numeric field id, with no generated bindings and no schema at hand, and}
 @item{a @emph{low-level builder} that follows the reference builder's
   write-back-to-front model, with vtable deduplication and correct
   per-primitive alignment.}]

There is no schema compiler and no code generation in this version; schema
codegen bindings are future work.  There is no FFI.  The only dependency is
@racket[base].

The format implemented here is the @emph{core} FlatBuffers format as
produced and consumed by current @tt{flatc} releases: scalars are always
@emph{little-endian}, buffers use @emph{32-bit} offsets, and buffers are
standard (not size-prefixed).  The 64-bit-offset variant of the format is
not supported.  (Little-endian is a property of the format itself, not of
this implementation: ``Each scalar is also always represented in
little-endian format.'')

@table-of-contents[]

@section{A tour of the format}

A FlatBuffers buffer starts with a @tt{uoffset} (a uint32) pointing forward
to the @emph{root table}.  A table starts with an @tt{soffset} (a signed
int32) such that @tt{vtable position = table position - soffset}; because
the value is signed, the vtable may lie before or after the table.  A
vtable holds a uint16 vtable size, a uint16 table inline size, and one
uint16 per field: the field's offset from the table start, or @litchar{0}
if the field is @emph{absent}.  Strings are uint32-length-prefixed,
null-terminated UTF-8 byte vectors; vectors are uint32-length-prefixed
sequences of aligned elements.  Strings, vectors, sub-tables and structs
are reached through unsigned offsets stored at the field's position, each
relative to its own location.

Two consequences drive this API:

@itemlist[
 @item{@emph{Default elision.}  A field whose value equals its default is
   simply not stored.  A reader must return the default for absent fields —
   which is why every accessor in this package takes an optional default.}
 @item{@emph{Vtable deduplication.}  Tables with identical layouts share
   one vtable; the shared vtable may sit @emph{after} the table (the
   @tt{soffset} is then negative).  The builder performs this dedup
   automatically, and the reader handles both directions.}]

@section{Quick start}

Take this schema (the one used by the package's own tests):

@verbatim[#:indent 2]|{
// test/schema.fbs (excerpt)
table Inner {
  x:    int32;     // id 0
  y:    int32;     // id 1
  name: string;    // id 2
}

table Outer {
  label:  string;    // id 0
  inner:  Inner;     // id 1
  inners: [Inner];   // id 2
  tags:   [string];  // id 3
  scalars: Scalars;  // id 4
}
}|

Building the equivalent buffer with the low-level builder.  Strings and
vectors must be created before the object that references them; vector
elements are prepended in @emph{reverse} order; field ids are the vtable
slot numbers:

@examples[#:eval shared-eval #:label #f
  (define b (make-builder))
  (define label-off (builder-create-string b "hello"))
  (define name-off (builder-create-string b "in"))
  (define a-off (builder-create-string b "a"))
  (define b-off (builder-create-string b "b"))
  (define inner-off
    (let ()
      (builder-start-object b 3)             ; Inner has 3 field slots
      (builder-add-i32-slot b 0 1)           ; x:  int32
      (builder-add-i32-slot b 1 -2)          ; y:  int32
      (builder-add-slot b 2 name-off)        ; name: string
      (builder-end-object b)))
  (define inner-a-off
    (let ()
      (builder-start-object b 3)
      (builder-add-i32-slot b 0 10)
      (builder-add-slot b 2 a-off)
      (builder-end-object b)))
  (builder-start-vector b 2 4)               ; 2 uoffset elements
  (builder-prepend-uoffset b inner-a-off)    ; last element first
  (builder-prepend-uoffset b inner-off)
  (define inners-off (builder-end-vector b 2))
  (define tags-off
    (let ()
      (builder-start-vector b 2 4)
      (builder-prepend-uoffset b b-off)      ; tags: ["a", "b"]
      (builder-prepend-uoffset b a-off)
      (builder-end-vector b 2)))
  (define root-off
    (let ()
      (builder-start-object b 5)             ; Outer has 5 field slots
      (builder-add-slot b 0 label-off)
      (builder-add-slot b 1 inner-off)
      (builder-add-slot b 2 inners-off)
      (builder-add-slot b 3 tags-off)
      (builder-add-slot b 4 0)               ; omitted (offset 0 = absent)
      (builder-end-object b)))
  (define buf (builder-finish b root-off))
  (bytes-length buf)]

Reading it back with the generic reader.  Missing fields return the
caller's default (or the type default @racket[0] / @racket[0.0] /
@racket[#f]):

@examples[#:eval shared-eval #:label #f
  (define t (fb-root (open-flatbuffer buf)))
  (table-field-string t 0)
  (table-field-i32 (table-field-table t 1) 1)
  (define inners (table-field-vector t 2))
  (fb-vector-length inners)
  (table-field-i32 (fb-vector-ref-table inners 1) 0)
  (table-field-table t 4)                    ; absent -> #f
  (table-field-i32 t 4 99)                   ; absent -> caller default
  (table-field-present? t 4)]

Buffers built by this builder are ordinary FlatBuffers buffers: they can be
verified with @tt{flatc -t}, read by the official C++/Java/Python runtimes,
and so on.  The package's test suite does exactly that (see
@secref["interop"]).

@section{Reading buffers}

@defproc[(open-flatbuffer [bs bytes?]) fb?]{
 Validates that @racket[bs] is large enough for a root offset, that the
 root offset is in range, and that the root table and its vtable are
 readable; raises @racket[exn:fail] on garbage.  The byte string is
 referenced zero-copy — it must not be mutated for as long as any value
 derived from the buffer is used.}

@defproc[(fb-root [f fb?]) table?]{
 The root table of the buffer.}

@defproc[(fb-bytes [f fb?]) bytes?]{
 The underlying byte string (useful for custom zero-copy parsing).}

A @racket[table] pairs a buffer with the table's absolute byte position;
@racket[table-pos] exposes it.  Fields are accessed by numeric field id
(the vtable slot, normally the field's declaration order in the schema):

@defproc[(table-field-present? [t table?] [id exact-nonnegative-integer?]) boolean?]{
 Is field @racket[id] present?  Per the format, a slot beyond the vtable's
 field count counts as absent — this is not an error.}

@defproc*[(
 [(table-field-bool [t table?] [id exact-nonnegative-integer?]
                    [default any/c #f]) boolean?]
 [(table-field-u8 [t table?] [id exact-nonnegative-integer?] [default any/c 0]) any/c]
 [(table-field-u16 [t table?] [id exact-nonnegative-integer?] [default any/c 0]) any/c]
 [(table-field-u32 [t table?] [id exact-nonnegative-integer?] [default any/c 0]) any/c]
 [(table-field-u64 [t table?] [id exact-nonnegative-integer?] [default any/c 0]) any/c]
 [(table-field-i8 [t table?] [id exact-nonnegative-integer?] [default any/c 0]) any/c]
 [(table-field-i16 [t table?] [id exact-nonnegative-integer?] [default any/c 0]) any/c]
 [(table-field-i32 [t table?] [id exact-nonnegative-integer?] [default any/c 0]) any/c]
 [(table-field-i64 [t table?] [id exact-nonnegative-integer?] [default any/c 0]) any/c]
 [(table-field-f32 [t table?] [id exact-nonnegative-integer?] [default any/c 0.0]) any/c]
 [(table-field-f64 [t table?] [id exact-nonnegative-integer?] [default any/c 0.0]) any/c]
 [(table-field-string [t table?] [id exact-nonnegative-integer?] [default any/c #f]) any/c]
 [(table-field-table [t table?] [id exact-nonnegative-integer?] [default any/c #f]) any/c]
)]{
 Typed field accessors.  If the field is absent, @racket[default] is
 returned — this is how FlatBuffers default elision reaches the reader.
 Strings are decoded as UTF-8 (invalid UTF-8 raises @racket[exn:fail]).}

@defproc[(table-field-vector [t table?] [id exact-nonnegative-integer?]
                             [default any/c #f]
                             [#:elem-size elem-size (or/c #f exact-positive-integer?) #f])
         any/c]{
 Returns an @racket[fb-vector] for the field, or @racket[default] if
 absent.  A generic reader cannot know an element's type, so the typed
 element accessors below decide the interpretation — use the accessor that
 matches the schema.  When @racket[elem-size] is given, the vector's byte
 span is validated eagerly.}

@defproc[(table-field-struct [t table?] [id exact-nonnegative-integer?]
                             [size exact-positive-integer?] [default any/c #f])
         any/c]{
 Inline struct fields are returned as a copied byte string of @racket[size]
 bytes (little-endian layout); the size must be known from the schema.  For
 zero-copy access, combine @racket[table-field-present?] with
 @racket[fb-bytes].}

@defstruct*[fb-vector ([fb fb?] [pos exact-nonnegative-integer?]
                       [len exact-nonnegative-integer?])]{
 A vector in the buffer; @racket[pos] is the absolute position of the first
 element.}

@defproc[(fb-vector-length [v fb-vector?]) exact-nonnegative-integer?]{
 Element count.}

@defproc*[(
 [(fb-vector-ref-u8 [v fb-vector?] [i exact-nonnegative-integer?]) (integer-in 0 255)]
 [(fb-vector-ref-u16 [v fb-vector?] [i exact-nonnegative-integer?]) (integer-in 0 65535)]
 [(fb-vector-ref-u32 [v fb-vector?] [i exact-nonnegative-integer?]) exact-nonnegative-integer?]
 [(fb-vector-ref-u64 [v fb-vector?] [i exact-nonnegative-integer?]) exact-nonnegative-integer?]
 [(fb-vector-ref-i8 [v fb-vector?] [i exact-nonnegative-integer?]) (integer-in -128 127)]
 [(fb-vector-ref-i16 [v fb-vector?] [i exact-nonnegative-integer?]) (integer-in -32768 32767)]
 [(fb-vector-ref-i32 [v fb-vector?] [i exact-nonnegative-integer?]) exact-integer?]
 [(fb-vector-ref-i64 [v fb-vector?] [i exact-nonnegative-integer?]) exact-integer?]
 [(fb-vector-ref-f32 [v fb-vector?] [i exact-nonnegative-integer?]) real?]
 [(fb-vector-ref-f64 [v fb-vector?] [i exact-nonnegative-integer?]) real?]
 [(fb-vector-ref-bool [v fb-vector?] [i exact-nonnegative-integer?]) boolean?]
 [(fb-vector-ref-string [v fb-vector?] [i exact-nonnegative-integer?]) string?]
 [(fb-vector-ref-table [v fb-vector?] [i exact-nonnegative-integer?]) table?]
)]{
 Typed element accessors, bounds-checked against the vector length and the
 buffer.}

@section{Building buffers}

@(define (builder-note) @list{All offsets returned by the procedures below
 are opaque non-negative integers usable as field values in
 @racket[builder-add-slot].})

@defproc[(make-builder [capacity exact-positive-integer? 1024]) builder?]{
 An empty builder.  The internal buffer grows as needed.}

@defproc[(builder-offset [b builder?]) exact-nonnegative-integer?]{
 The number of bytes written so far; this is the builder's ``offset''
 currency.}

@defproc[(builder-create-string [b builder?] [str string?]) exact-nonnegative-integer?]{
 Encodes @racket[str] as UTF-8 with the length prefix and NUL terminator
 (@tt{[u32 length][bytes][NUL][padding]}), aligned so the length prefix is
 4-byte aligned.  Must not be called while an object or vector is under
 construction.}

@defproc[(builder-create-u8-vector [b builder?] [data bytes?]) exact-nonnegative-integer?]{
 Creates a @tt{[ubyte]} vector.}

@defproc[(builder-start-vector [b builder?] [num-elems exact-nonnegative-integer?]
                               [elem-size exact-positive-integer?]) void?]{
 Begins a vector; then prepend the elements in @emph{reverse} order with
 the @racket[builder-prepend-*] primitives, and call
 @racket[builder-end-vector] with the same @racket[num-elems].
 @racket[elem-size] is the per-element byte size (4 for offset vectors,
 otherwise the scalar's size).}

@defproc[(builder-end-vector [b builder?] [num-elems exact-nonnegative-integer?])
         exact-nonnegative-integer?]{
 Writes the length prefix and returns the vector's offset.}

@defproc[(builder-start-object [b builder?] [num-fields exact-nonnegative-integer?]) void?]{
 Begins an object with @racket[num-fields] vtable slots; field ids used by
 the @tt{builder-add-*-slot} procedures must be smaller than
 @racket[num-fields].}

@defproc[(builder-add-slot [b builder?] [field-id exact-nonnegative-integer?]
                           [offset-value exact-nonnegative-integer?]) void?]{
 Adds an offset field (string, vector or table): a @tt{uoffset} pointing
 at @racket[offset-value].  Passing @racket[0] omits the field.}

@defproc*[(
 [(builder-add-bool-slot [b builder?] [id exact-nonnegative-integer?] [v any/c]
                         [default any/c #f]) void?]
 [(builder-add-u8-slot [b builder?] [id exact-nonnegative-integer?] [v (integer-in 0 255)]
                       [default (integer-in 0 255) 0]) void?]
 [(builder-add-u16-slot [b builder?] [id exact-nonnegative-integer?] [v (integer-in 0 65535)]
                        [default (integer-in 0 65535) 0]) void?]
 [(builder-add-u32-slot [b builder?] [id exact-nonnegative-integer?] [v exact-nonnegative-integer?]
                        [default exact-nonnegative-integer? 0]) void?]
 [(builder-add-u64-slot [b builder?] [id exact-nonnegative-integer?] [v exact-nonnegative-integer?]
                        [default exact-nonnegative-integer? 0]) void?]
 [(builder-add-i8-slot [b builder?] [id exact-nonnegative-integer?] [v (integer-in -128 127)]
                       [default (integer-in -128 127) 0]) void?]
 [(builder-add-i16-slot [b builder?] [id exact-nonnegative-integer?] [v (integer-in -32768 32767)]
                        [default (integer-in -32768 32767) 0]) void?]
 [(builder-add-i32-slot [b builder?] [id exact-nonnegative-integer?] [v exact-integer?]
                        [default exact-integer? 0]) void?]
 [(builder-add-i64-slot [b builder?] [id exact-nonnegative-integer?] [v exact-integer?]
                        [default exact-integer? 0]) void?]
 [(builder-add-f32-slot [b builder?] [id exact-nonnegative-integer?] [v real?]
                        [default real? 0.0]) void?]
 [(builder-add-f64-slot [b builder?] [id exact-nonnegative-integer?] [v real?]
                        [default real? 0.0]) void?]
)]{
 Typed slot writers with @emph{default elision}: a value @racket[equal?]
 to @racket[default] is not written at all, exactly matching the reference
 builders and the reader's fallback semantics.  (NaN never equals its
 default, so NaN fields are always written.)}

@defproc[(builder-end-object [b builder?]) exact-nonnegative-integer?]{
 Writes the table: the soffset placeholder, then the vtable (deduplicated
 against previously written vtables), then patches the soffset.  The
 returned offset is the table's.}

@defproc[(builder-finish [b builder?] [root exact-nonnegative-integer?]) bytes?]{
 Aligns the total length to the largest alignment used, prepends the root
 offset, and returns the finished byte string: a standard 32-bit,
 little-endian, non-size-prefixed FlatBuffers buffer.}

@subsection{Alignment primitives}

The @tt{builder-prepend-*} primitives write scalars back-to-front, each
aligned to its own size relative to the start of the finished buffer.  The
two padding primitives are exposed for completeness:

@defproc[(builder-prep [b builder?] [alignment exact-positive-integer?]) void?]{
 Pads so the current offset becomes a multiple of @racket[alignment].}

@defproc[(builder-prealign [b builder?] [future-bytes exact-nonnegative-integer?]
                           [alignment exact-positive-integer?]) void?]{
 Pads so the offset becomes aligned after @racket[future-bytes] further
 bytes are written — the primitive behind string and vector prefix
 alignment.}

@section[#:tag "interop"]{Interoperability with flatc}

The test suite (@filepath{flatbuffers/test/flatbuffers-test.rkt}) checks:

@itemlist[
 @item{binaries produced by @tt{flatc -b} from JSON test data (including a
   default-elision case and an empty-vector/empty-string edge case) are
   decoded correctly by the reader;}
 @item{buffers built by the builder are converted back to JSON with
   @tt{flatc -t} and compared field-by-field semantically — byte-for-byte
   equality is not required, because the format deliberately leaves field
   order, object placement and vtable sharing unspecified;}
 @item{an alignment fuzz loop (strings of length 1..40 interleaved with
   uint64 fields) round-trips through both our reader and, for one case,
   flatc;}
 @item{vtable deduplication produces buffers flatc accepts (soffset
   pointing forward to a shared vtable).}]

The interop tests are skipped gracefully at runtime when @tt{flatc} is not
on @envvar{PATH} (e.g. on Windows CI), so the pure-Racket round-trip suite
always runs.

@section{Limitations}

@itemlist[
 @item{Little-endian, 32-bit-offset, non-size-prefixed buffers only.}
 @item{No schema verification: a generic reader trusts the schema knowledge
   of its caller (a vector's element type, a struct's size, ...).
   Malformed or hostile buffers raise @racket[exn:fail] on out-of-range
   access rather than guaranteeing memory safety properties beyond
   Racket's own byte-string bounds.  A formal verifier is future work.}
 @item{The builder deduplicates vtables byte-exactly (like the reference
   implementation); tables built with different slot counts produce
   distinct vtables even if some slots are unused.}]
