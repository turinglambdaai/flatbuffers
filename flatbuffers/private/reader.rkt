#lang racket/base

;; Low-level, generic FlatBuffers reader.
;;
;; Reads the core FlatBuffers wire format directly from a byte string,
;; without any schema codegen:
;;
;;   - little-endian scalars only (the core format is little-endian),
;;   - 32-bit offsets only (`uoffset_t` / `soffset_t`),
;;   - vtable-driven table field access by numeric field id,
;;   - zero-copy: the input byte string is referenced, never copied; all
;;     positions are absolute byte indices into it.
;;
;; Format reference: https://flatbuffers.dev/internals/
;;   * A buffer starts with a uoffset (uint32) pointing forward to the root
;;     table.
;;   * A table starts with an soffset (int32) such that
;;     `vtable position = table position - soffset` (signed; the vtable may
;;     lie before or after the table).
;;   * A vtable is: uint16 vtable-size-in-bytes, uint16 table-inline-size,
;;     then one uint16 per field: the field's offset from the table start,
;;     or 0 if the field is absent (default elision).
;;   * Strings are u32-length-prefixed, null-terminated byte vectors.
;;   * Vectors are u32-length-prefixed sequences of aligned elements;
;;     elements are scalars or uoffsets (strings/tables).

(require racket/contract)

(provide
 (contract-out
  [struct fb ([bs bytes?])]
  [open-flatbuffer (-> bytes? fb?)]
  [fb-bytes (-> fb? bytes?)]
  [fb-root (-> fb? table?)]
  [struct table ([fb fb?] [pos exact-nonnegative-integer?])]
  [table-field-present? (-> table? field-id/c boolean?)]
  [table-field-bool (->* (table? field-id/c) (any/c) boolean?)]
  [table-field-u8 (->* (table? field-id/c) (any/c) any/c)]
  [table-field-u16 (->* (table? field-id/c) (any/c) any/c)]
  [table-field-u32 (->* (table? field-id/c) (any/c) any/c)]
  [table-field-u64 (->* (table? field-id/c) (any/c) any/c)]
  [table-field-i8 (->* (table? field-id/c) (any/c) any/c)]
  [table-field-i16 (->* (table? field-id/c) (any/c) any/c)]
  [table-field-i32 (->* (table? field-id/c) (any/c) any/c)]
  [table-field-i64 (->* (table? field-id/c) (any/c) any/c)]
  [table-field-f32 (->* (table? field-id/c) (any/c) any/c)]
  [table-field-f64 (->* (table? field-id/c) (any/c) any/c)]
  [table-field-string (->* (table? field-id/c) (any/c) any/c)]
  [table-field-table (->* (table? field-id/c) (any/c) any/c)]
  [table-field-vector
   (->* (table? field-id/c)
        (any/c #:elem-size (or/c #f exact-positive-integer?))
        any/c)]
  [table-field-struct
   (->* (table? field-id/c exact-positive-integer?) (any/c) any/c)]
  [struct fb-vector
   ([fb fb?] [pos exact-nonnegative-integer?] [len exact-nonnegative-integer?])]
  [fb-vector-length (-> fb-vector? exact-nonnegative-integer?)]
  [fb-vector-ref-u8 (-> fb-vector? exact-nonnegative-integer? (integer-in 0 255))]
  [fb-vector-ref-u16 (-> fb-vector? exact-nonnegative-integer? (integer-in 0 65535))]
  [fb-vector-ref-u32 (-> fb-vector? exact-nonnegative-integer? exact-nonnegative-integer?)]
  [fb-vector-ref-u64 (-> fb-vector? exact-nonnegative-integer? exact-nonnegative-integer?)]
  [fb-vector-ref-i8 (-> fb-vector? exact-nonnegative-integer? (integer-in -128 127))]
  [fb-vector-ref-i16 (-> fb-vector? exact-nonnegative-integer? (integer-in -32768 32767))]
  [fb-vector-ref-i32 (-> fb-vector? exact-nonnegative-integer? exact-integer?)]
  [fb-vector-ref-i64 (-> fb-vector? exact-nonnegative-integer? exact-integer?)]
  [fb-vector-ref-f32 (-> fb-vector? exact-nonnegative-integer? real?)]
  [fb-vector-ref-f64 (-> fb-vector? exact-nonnegative-integer? real?)]
  [fb-vector-ref-bool (-> fb-vector? exact-nonnegative-integer? boolean?)]
  [fb-vector-ref-string (-> fb-vector? exact-nonnegative-integer? string?)]
  [fb-vector-ref-table (-> fb-vector? exact-nonnegative-integer? table?)]))

;; A table field id (the vtable slot number).
(define field-id/c exact-nonnegative-integer?)

(define (raise-fb message)
  (raise (exn:fail message (current-continuation-marks))))

;; Checks that the byte range [start, start+n) is inside `bs`.
(define (require-range bs start n what)
  (unless (and (exact-nonnegative-integer? start)
               (exact-nonnegative-integer? n)
               (<= n (- (bytes-length bs) start)))
    (raise-fb
     (format "flatbuffers: ~a at byte offset ~a (~a byte(s)) lies outside the buffer (length ~a)"
             what start n (bytes-length bs)))))

;; ---- little-endian scalar reads ------------------------------------------

(define (u8-at bs i) (bytes-ref bs i))
(define (u16-at bs i) (integer-bytes->integer bs #f #f i (+ i 2)))
(define (u32-at bs i) (integer-bytes->integer bs #f #f i (+ i 4)))
(define (u64-at bs i) (integer-bytes->integer bs #f #f i (+ i 8)))
(define (i8-at bs i) (integer-bytes->integer bs #t #f i (+ i 1)))
(define (i16-at bs i) (integer-bytes->integer bs #t #f i (+ i 2)))
(define (i32-at bs i) (integer-bytes->integer bs #t #f i (+ i 4)))
(define (i64-at bs i) (integer-bytes->integer bs #t #f i (+ i 8)))
(define (f32-at bs i) (floating-point-bytes->real (subbytes bs i (+ i 4)) #f))
(define (f64-at bs i) (floating-point-bytes->real (subbytes bs i (+ i 8)) #f))

;; ---- buffers and tables ---------------------------------------------------

(struct fb (bs))
(struct table (fb pos))
(struct fb-vector (fb pos len))

;; Public aliases with stable names.
(define (fb-bytes f) (fb-bs f))

;; Public alias: the struct field is `len` so the accessor does not clash
;; with this explicitly contracted function of the same name.
(define (fb-vector-length v) (fb-vector-len v))

;; Opens a byte string as a FlatBuffers buffer, validating that it is at
;; least large enough to hold the root offset and that the root table and
;; its vtable are in range.  Raises `exn:fail` on garbage.
(define (open-flatbuffer bs)
  (unless (bytes? bs)
    (raise-argument-error 'open-flatbuffer "bytes?" bs))
  (define len (bytes-length bs))
  (when (< len 4)
    (raise-fb
     (format "flatbuffers: buffer too small (~a byte(s)); it must at least hold the 4-byte root offset" len)))
  (define root (u32-at bs 0))
  (when (> root (- len 4))
    (raise-fb
     (format "flatbuffers: root offset ~a is outside the buffer (length ~a)" root len)))
  ;; Fail fast on garbage: the root table and its vtable must be readable.
  (void (table-vtable-pos (table (fb bs) root)))
  (fb bs))

(define (fb-root f)
  (define bs (fb-bs f))
  (define root (u32-at bs 0))
  (require-range bs root 4 "root table")
  (table f root))

;; Absolute position of the table's vtable; raises if it points outside the
;; buffer or carries a nonsense size.
(define (table-vtable-pos t)
  (define bs (fb-bs (table-fb t)))
  (define tp (table-pos t))
  (require-range bs tp 4 "table start")
  (define soffset (i32-at bs tp))
  (define vtp (- tp soffset))
  (when (or (negative? vtp) (> (+ vtp 4) (bytes-length bs)))
    (raise-fb
     (format "flatbuffers: vtable offset ~a (table at ~a, soffset ~a) points outside the buffer (length ~a)"
             vtp tp soffset (bytes-length bs))))
  (define vtsize (u16-at bs vtp))
  (when (< vtsize 4)
    (raise-fb
     (format "flatbuffers: vtable at byte offset ~a declares invalid size ~a" vtp vtsize)))
  (require-range bs vtp vtsize "vtable")
  vtp)

;; Absolute position of field `id`, or #f if the field is absent.
;; Per the format: a vtable slot beyond the vtable's field count, or a slot
;; whose entry is 0, means the field is absent.
(define (table-field-pos t id)
  (unless (field-id/c id)
    (raise-argument-error 'table-field "exact-nonnegative-integer? (field id)" id))
  (define vtp (table-vtable-pos t))
  (define bs (fb-bs (table-fb t)))
  (define vtsize (u16-at bs vtp))
  (define byte-offset (+ 4 (* 2 id)))
  (and (< byte-offset vtsize)
       (let ([rel (u16-at bs (+ vtp byte-offset))])
         (and (> rel 0)
              (let ([p (+ (table-pos t) rel)])
                (require-range bs p 1 "table field")
                p)))))

(define (table-field-present? t id)
  (not (not (table-field-pos t id))))

;; ---- scalar fields ---------------------------------------------------------

;; Factory for the optional-default access pattern: an absent field yields
;; the default (caller-supplied or the type's default), which is exactly how
;; FlatBuffers default elision works on the wire.
(define (make-scalar-field-reader size read type-default)
  (case-lambda
    [(t id) (read-scalar-field t id size read type-default)]
    [(t id default) (read-scalar-field t id size read default)]))

(define (read-scalar-field t id size read default)
  (define p (table-field-pos t id))
  (if (not p)
      default
      (let ([bs (fb-bs (table-fb t))])
        (require-range bs p size (format "value of field id ~a" id))
        (read bs p))))

(define table-field-bool (make-scalar-field-reader 1 (lambda (bs i) (not (zero? (u8-at bs i)))) #f))
(define table-field-u8 (make-scalar-field-reader 1 u8-at 0))
(define table-field-u16 (make-scalar-field-reader 2 u16-at 0))
(define table-field-u32 (make-scalar-field-reader 4 u32-at 0))
(define table-field-u64 (make-scalar-field-reader 8 u64-at 0))
(define table-field-i8 (make-scalar-field-reader 1 i8-at 0))
(define table-field-i16 (make-scalar-field-reader 2 i16-at 0))
(define table-field-i32 (make-scalar-field-reader 4 i32-at 0))
(define table-field-i64 (make-scalar-field-reader 8 i64-at 0))
(define table-field-f32 (make-scalar-field-reader 4 f32-at 0.0))
(define table-field-f64 (make-scalar-field-reader 8 f64-at 0.0))

;; ---- indirect fields (strings, tables, vectors, structs) -------------------

;; Resolves the uoffset stored at field position `p` and returns the absolute
;; target position (validated to at least hold a length prefix).
(define (indirect-target t p what)
  (define bs (fb-bs (table-fb t)))
  (require-range bs p 4 (format "offset of ~a field id" what))
  (define target (+ p (u32-at bs p)))
  (require-range bs target 4 (format "start of ~a" what))
  target)

;; Reads the UTF-8 string starting at absolute position `q` (which points at
;; the u32 length prefix).  Raises `exn:fail` on invalid UTF-8.
(define (read-string-at fb q)
  (define bs (fb-bs fb))
  (define n (u32-at bs q))
  (require-range bs (+ q 4) n "string bytes")
  (bytes->string/utf-8 (subbytes bs (+ q 4) (+ q 4 n))))

(define (table-field-string t id [default #f])
  (define p (table-field-pos t id))
  (if (not p)
      default
      (read-string-at (table-fb t) (indirect-target t p "string"))))

(define (table-field-table t id [default #f])
  (define p (table-field-pos t id))
  (if (not p)
      default
      (let ([target (indirect-target t p "table")])
        (define nt (table (table-fb t) target))
        (void (table-vtable-pos nt)) ; validate eagerly
        nt)))

(define (table-field-vector t id [default #f] #:elem-size [elem-size #f])
  (define p (table-field-pos t id))
  (if (not p)
      default
      (let ([q (indirect-target t p "vector")])
        (define bs (fb-bs (table-fb t)))
        (define n (u32-at bs q))
        (when elem-size
          (require-range bs (+ q 4) (* n elem-size) "vector elements"))
        (fb-vector (table-fb t) (+ q 4) n))))

;; Fixed-size inline struct: returned as a copied byte string (little-endian
;; layout).  `size` must be known from the schema.  For zero-copy access,
;; combine `table-field-present?` with `fb-bytes` and `table-pos`.
(define (table-field-struct t id size [default #f])
  (define p (table-field-pos t id))
  (if (not p)
      default
      (let ([bs (fb-bs (table-fb t))])
        (require-range bs p size "inline struct")
        (subbytes bs p (+ p size)))))

;; ---- vector element access --------------------------------------------------

(define (fb-vector-ref v idx size read what)
  (unless (exact-nonnegative-integer? idx)
    (raise-argument-error 'fb-vector-ref "exact-nonnegative-integer? (element index)" idx))
  (unless (< idx (fb-vector-length v))
    (raise-fb
     (format "flatbuffers: vector index ~a out of range (vector length ~a)"
             idx (fb-vector-length v))))
  (define bs (fb-bs (fb-vector-fb v)))
  (define p (+ (fb-vector-pos v) (* idx size)))
  (require-range bs p size what)
  (read bs p))

(define (fb-vector-ref-u8 v i) (fb-vector-ref v i 1 u8-at "u8 vector element"))
(define (fb-vector-ref-u16 v i) (fb-vector-ref v i 2 u16-at "u16 vector element"))
(define (fb-vector-ref-u32 v i) (fb-vector-ref v i 4 u32-at "u32 vector element"))
(define (fb-vector-ref-u64 v i) (fb-vector-ref v i 8 u64-at "u64 vector element"))
(define (fb-vector-ref-i8 v i) (fb-vector-ref v i 1 i8-at "i8 vector element"))
(define (fb-vector-ref-i16 v i) (fb-vector-ref v i 2 i16-at "i16 vector element"))
(define (fb-vector-ref-i32 v i) (fb-vector-ref v i 4 i32-at "i32 vector element"))
(define (fb-vector-ref-i64 v i) (fb-vector-ref v i 8 i64-at "i64 vector element"))
(define (fb-vector-ref-f32 v i) (fb-vector-ref v i 4 f32-at "f32 vector element"))
(define (fb-vector-ref-f64 v i) (fb-vector-ref v i 8 f64-at "f64 vector element"))
(define (fb-vector-ref-bool v i)
  (not (zero? (fb-vector-ref v i 1 u8-at "bool vector element"))))

;; Vector of strings: elements are uoffsets relative to their own position.
(define (fb-vector-ref-string v i)
  (define fb (fb-vector-fb v))
  (define ep (fb-vector-ref v i 4 (lambda (bs p) p) "string offset element"))
  (read-string-at fb (indirect-vector-target fb ep "string")))

;; Vector of tables: elements are uoffsets relative to their own position.
(define (fb-vector-ref-table v i)
  (define fb (fb-vector-fb v))
  (define ep (fb-vector-ref v i 4 (lambda (bs p) p) "table offset element"))
  (define nt (table fb (indirect-vector-target fb ep "table")))
  (void (table-vtable-pos nt)) ; validate eagerly
  nt)

;; Same as `indirect-target` but for a position inside a vector.
(define (indirect-vector-target fb p what)
  (define bs (fb-bs fb))
  (require-range bs p 4 (format "offset of ~a vector element" what))
  (define target (+ p (u32-at bs p)))
  (require-range bs target 4 (format "start of ~a" what))
  target)
