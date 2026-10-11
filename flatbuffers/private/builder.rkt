#lang racket/base

;; Low-level FlatBuffers builder.
;;
;; Follows the reference builder's model exactly (see
;; https://flatbuffers.dev/internals/ and flatbuffer_builder.h):
;;
;;   - Bytes are written back-to-front: everything is "prepended", so the
;;     first byte written ends up at the highest address and the last byte
;;     written is the buffer's first byte (the root offset).
;;   - Before every aligned write, the current write position is padded so
;;     that scalars end up aligned to their own size *relative to the start
;;     of the finished buffer*.
;;   - Tables: `builder-start-object` ... field writes ... `builder-end-object`
;;     writes the soffset placeholder, then the vtable, then patches the
;;     soffset.  Identical vtables are deduplicated: the second table's
;;     soffset points at the already-written vtable (it may point forward,
;;     i.e. have a negative value, because earlier-written data sits at
;;     higher addresses).
;;   - Fields equal to their default value are simply not written (default
;;     elision): readers return the default for absent fields.
;;   - 32-bit offsets, little-endian, standard (non-size-prefixed) buffers.

(require racket/contract)

;; Opaque builder state: buffer, write head (next free index, decreasing),
;; the smallest alignment any write required, the vtable dedup table
;; (vtable bytes -> absolute written-position of the vtable), and the state
;; of the object/vector under construction.
(struct builder (buf head minalign vtables nested obj-num-fields obj-fields obj-start vec-count)
  #:mutable)

(provide
 (contract-out
  [builder? (-> any/c boolean?)]
  [make-builder (->* () (exact-positive-integer?) builder?)]
  [builder-offset (-> builder? exact-nonnegative-integer?)]
  [builder-prep (-> builder? exact-positive-integer? void?)]
  [builder-prealign (-> builder? exact-nonnegative-integer? exact-positive-integer? void?)]
  [builder-prepend-u8 (-> builder? (integer-in 0 255) void?)]
  [builder-prepend-i8 (-> builder? (integer-in -128 127) void?)]
  [builder-prepend-u16 (-> builder? (integer-in 0 65535) void?)]
  [builder-prepend-i16 (-> builder? (integer-in -32768 32767) void?)]
  [builder-prepend-u32 (-> builder? exact-nonnegative-integer? void?)]
  [builder-prepend-i32 (-> builder? exact-integer? void?)]
  [builder-prepend-u64 (-> builder? exact-nonnegative-integer? void?)]
  [builder-prepend-i64 (-> builder? exact-integer? void?)]
  [builder-prepend-f32 (-> builder? real? void?)]
  [builder-prepend-f64 (-> builder? real? void?)]
  [builder-prepend-uoffset (-> builder? exact-nonnegative-integer? void?)]
  [builder-create-string (-> builder? string? exact-nonnegative-integer?)]
  [builder-create-u8-vector (-> builder? bytes? exact-nonnegative-integer?)]
  [builder-start-vector (-> builder? exact-nonnegative-integer? exact-positive-integer? void?)]
  [builder-end-vector (-> builder? exact-nonnegative-integer? exact-nonnegative-integer?)]
  [builder-start-object (-> builder? exact-nonnegative-integer? void?)]
  [builder-end-object (-> builder? exact-nonnegative-integer?)]
  [builder-add-slot (-> builder? exact-nonnegative-integer? exact-nonnegative-integer? void?)]
  [builder-add-bool-slot
   (->* (builder? exact-nonnegative-integer? any/c) (any/c) void?)]
  [builder-add-u8-slot (->* (builder? exact-nonnegative-integer? (integer-in 0 255))
                            ((integer-in 0 255)) void?)]
  [builder-add-u16-slot (->* (builder? exact-nonnegative-integer? (integer-in 0 65535))
                             ((integer-in 0 65535)) void?)]
  [builder-add-u32-slot (->* (builder? exact-nonnegative-integer? exact-nonnegative-integer?)
                             (exact-nonnegative-integer?) void?)]
  [builder-add-u64-slot (->* (builder? exact-nonnegative-integer? exact-nonnegative-integer?)
                             (exact-nonnegative-integer?) void?)]
  [builder-add-i8-slot (->* (builder? exact-nonnegative-integer? (integer-in -128 127))
                            ((integer-in -128 127)) void?)]
  [builder-add-i16-slot (->* (builder? exact-nonnegative-integer? (integer-in -32768 32767))
                             ((integer-in -32768 32767)) void?)]
  [builder-add-i32-slot (->* (builder? exact-nonnegative-integer? exact-integer?)
                             (exact-integer?) void?)]
  [builder-add-i64-slot (->* (builder? exact-nonnegative-integer? exact-integer?)
                             (exact-integer?) void?)]
  [builder-add-f32-slot (->* (builder? exact-nonnegative-integer? real?) (real?) void?)]
  [builder-add-f64-slot (->* (builder? exact-nonnegative-integer? real?) (real?) void?)]
  [builder-finish (-> builder? exact-nonnegative-integer? bytes?)]))

(define (raise-fb message)
  (raise (exn:fail message (current-continuation-marks))))

(define (make-builder [capacity 1024])
  (builder (make-bytes capacity 0)
           capacity ; head: next free byte, decreasing
           1        ; minalign: largest alignment used so far
           (make-hash) ; vtable dedup: vtable bytes -> written-position
           #f      ; nested: #f | 'object | 'vector
           #f      ; obj-num-fields
           #f      ; obj-fields: hash field-id -> written-position
           #f      ; obj-start: written-position where the object began
           #f))    ; vec-count: element count of the vector under construction

;; Number of bytes written so far; this doubles as the value of "offsets":
;; a byte written when the offset is W sits at address (final-length - W).
(define (builder-offset b)
  (- (bytes-length (builder-buf b)) (builder-head b)))

(define (ensure-capacity b n)
  (when (< (builder-head b) n)
    (define old (builder-buf b))
    (define old-len (bytes-length old))
    (define new-len (max (* 2 old-len) (+ old-len n)))
    (define new-buf (make-bytes new-len 0))
    (bytes-copy! new-buf (- new-len old-len) old)
    (set-builder-buf! b new-buf)
    (set-builder-head! b (+ (builder-head b) (- new-len old-len)))))

;; Writes `bstr` so that its first byte lands at the current head; the write
;; position moves down and `builder-offset` grows by the block's length.
(define (prepend-bytes! b bstr)
  (define n (bytes-length bstr))
  (ensure-capacity b n)
  (set-builder-head! b (- (builder-head b) n))
  (bytes-copy! (builder-buf b) (builder-head b) bstr))

;; Pads with zeroes so that the current offset becomes a multiple of
;; `alignment`.  Every aligned write goes through here, which (together with
;; finishing at a multiple of minalign) makes all elements aligned relative
;; to the start of the finished buffer.
(define (builder-prep b alignment)
  (define a (max 1 alignment))
  (when (> a (builder-minalign b))
    (set-builder-minalign! b a))
  (define pad (modulo (- (builder-offset b)) a))
  (pad! b pad))

;; Pads so that the offset becomes a multiple of `alignment` after
;; `future-bytes` further bytes are written.  Used before writing the
;; variable-length payload of strings and vectors, so their length prefix
;; lands aligned without padding *between* prefix and payload.
(define (builder-prealign b future-bytes alignment)
  (define a (max 1 alignment))
  (when (> a (builder-minalign b))
    (set-builder-minalign! b a))
  (define pad (modulo (- (+ future-bytes (builder-offset b))) a))
  (pad! b pad))

(define (pad! b n)
  (unless (zero? n)
    (prepend-bytes! b (make-bytes n 0))))

;; ---- aligned primitive prepends -------------------------------------------

(define (check-uint v bits who)
  (unless (and (exact-nonnegative-integer? v) (< v (arithmetic-shift 1 bits)))
    (raise-fb
     (format "flatbuffers: ~a: value ~a does not fit in an unsigned ~a-bit integer" who v bits))))

(define (check-sint v bits who)
  (define limit (arithmetic-shift 1 (sub1 bits)))
  (unless (and (exact-integer? v) (<= (- limit) v (sub1 limit)))
    (raise-fb
     (format "flatbuffers: ~a: value ~a does not fit in a signed ~a-bit integer" who v bits))))

(define (builder-prepend-u8 b v)
  (check-uint v 8 'builder-prepend-u8)
  (builder-prep b 1)
  (prepend-bytes! b (bytes v)))

(define (builder-prepend-i8 b v)
  (check-sint v 8 'builder-prepend-i8)
  (builder-prep b 1)
  (prepend-bytes! b (integer->integer-bytes v 1 #t #f)))

(define (builder-prepend-u16 b v)
  (check-uint v 16 'builder-prepend-u16)
  (builder-prep b 2)
  (prepend-bytes! b (integer->integer-bytes v 2 #f #f)))

(define (builder-prepend-i16 b v)
  (check-sint v 16 'builder-prepend-i16)
  (builder-prep b 2)
  (prepend-bytes! b (integer->integer-bytes v 2 #t #f)))

(define (builder-prepend-u32 b v)
  (check-uint v 32 'builder-prepend-u32)
  (builder-prep b 4)
  (prepend-bytes! b (integer->integer-bytes v 4 #f #f)))

(define (builder-prepend-i32 b v)
  (check-sint v 32 'builder-prepend-i32)
  (builder-prep b 4)
  (prepend-bytes! b (integer->integer-bytes v 4 #t #f)))

(define (builder-prepend-u64 b v)
  (check-uint v 64 'builder-prepend-u64)
  (builder-prep b 8)
  (prepend-bytes! b (integer->integer-bytes v 8 #f #f)))

(define (builder-prepend-i64 b v)
  (check-sint v 64 'builder-prepend-i64)
  (builder-prep b 8)
  (prepend-bytes! b (integer->integer-bytes v 8 #t #f)))

(define (builder-prepend-f32 b v)
  (builder-prep b 4)
  (prepend-bytes! b (real->floating-point-bytes v 4 #f)))

(define (builder-prepend-f64 b v)
  (builder-prep b 8)
  (prepend-bytes! b (real->floating-point-bytes v 8 #f)))

;; A uoffset points forward from its own location to a string, vector or
;; table.  `off` is the target's builder offset (written-position token,
;; as returned by builder-create-string &c.); the stored value is the
;; target's address minus the uoffset's own address — in written-position
;; arithmetic, (current offset after prep + 4) - off, matching the
;; reference builder's ReferTo (size - off + sizeof(uoffset_t)).
(define (builder-prepend-uoffset b off)
  (check-uint off 32 'builder-prepend-uoffset)
  (unless (<= off (builder-offset b))
    (raise-fb
     (format "flatbuffers: builder-prepend-uoffset: offset ~a does not refer to data written earlier (current offset ~a)"
             off (builder-offset b))))
  (builder-prep b 4)
  (define value (- (+ (builder-offset b) 4) off))
  (check-uint value 32 'builder-prepend-uoffset)
  (prepend-bytes! b (integer->integer-bytes value 4 #f #f)))

;; ---- strings and vectors ----------------------------------------------------

(define (check-not-nested! b who)
  (when (builder-nested b)
    (raise-fb
     (format "flatbuffers: ~a must not be called while ~a is under construction"
             who (builder-nested b)))))

(define (check-mode! b expected who)
  (unless (eq? (builder-nested b) expected)
    (raise-fb
     (format "flatbuffers: ~a called without a matching ~a"
             who (if (eq? expected 'object) 'builder-start-object 'builder-start-vector)))))

;; Creates a UTF-8 string: [u32 length][bytes][NUL][alignment padding].
;; Returns the offset of the string (the value for a uoffset field).
(define (builder-create-string b str)
  (check-not-nested! b 'builder-create-string)
  (define bstr (string->bytes/utf-8 str))
  ;; After string + NUL are written, the position must be 4-aligned for the
  ;; length prefix; the padding ends up *after* the NUL terminator.
  (builder-prealign b (+ (bytes-length bstr) 1) 4)
  (prepend-bytes! b (bytes 0))            ; NUL terminator
  (prepend-bytes! b bstr)
  (builder-prepend-u32 b (bytes-length bstr))
  (builder-offset b))

;; Creates a [ubyte] vector from a byte string.
(define (builder-create-u8-vector b bstr)
  (check-not-nested! b 'builder-create-u8-vector)
  (builder-prealign b (bytes-length bstr) 4)
  (prepend-bytes! b bstr)
  (builder-prepend-u32 b (bytes-length bstr))
  (builder-offset b))

;; Begins a generic vector of `num-elems` elements of `elem-size` bytes each.
;; Afterwards, prepend the elements in REVERSE order (last element first),
;; using the builder-prepend-* primitives; then call builder-end-vector with
;; the same element count.
(define (builder-start-vector b num-elems elem-size)
  (check-not-nested! b 'builder-start-vector)
  ;; After all elements are written, the position must be aligned for the
  ;; length prefix (4) and for the elements themselves.
  (builder-prealign b (* num-elems elem-size) (max 4 elem-size))
  (set-builder-nested! b 'vector)
  (set-builder-vec-count! b num-elems))

(define (builder-end-vector b num-elems)
  (check-mode! b 'vector 'builder-end-vector)
  (unless (equal? num-elems (builder-vec-count b))
    (raise-fb
     (format "flatbuffers: builder-end-vector called with ~a elements but builder-start-vector was given ~a"
             num-elems (builder-vec-count b))))
  (builder-prepend-u32 b num-elems)
  (set-builder-nested! b #f)
  (set-builder-vec-count! b #f)
  (builder-offset b))

;; ---- tables ------------------------------------------------------------------

;; Begins an object with `num-fields` slots (the field ids used by
;; builder-add-*-slot must be strictly less than num-fields).
(define (builder-start-object b num-fields)
  (check-not-nested! b 'builder-start-object)
  (set-builder-nested! b 'object)
  (set-builder-obj-num-fields! b num-fields)
  (set-builder-obj-fields! b (make-hash))
  (set-builder-obj-start! b (builder-offset b)))

(define (check-slot! b id who)
  (check-mode! b 'object who)
  (unless (and (exact-nonnegative-integer? id) (< id (builder-obj-num-fields b)))
    (raise-fb
     (format "flatbuffers: ~a: field id ~a out of range (object has ~a field slot(s))"
             who id (builder-obj-num-fields b)))))

(define (record-field! b id)
  (hash-set! (builder-obj-fields b) id (builder-offset b)))

;; Adds an offset field (string / vector / table): prepends a uoffset to
;; `offset-value`.  Passing 0 omits the field entirely.
(define (builder-add-slot b field-id offset-value)
  (check-slot! b field-id 'builder-add-slot)
  (check-uint offset-value 32 'builder-add-slot)
  (unless (zero? offset-value)
    (builder-prepend-uoffset b offset-value)
    (record-field! b field-id)))

;; Typed slot writers with default elision: a value equal to `default`
;; (type default: 0, 0.0, #f) is simply not written; readers return the
;; default for absent fields.  NaN never equals its default, so NaN fields
;; are always written.
(define (add-scalar-slot b id v default prep!)
  (check-slot! b id 'builder-add-slot)
  (unless (equal? v default)
    (prep! b v)
    (record-field! b id)))

(define (builder-add-bool-slot b field-id v [default #f])
  (add-scalar-slot b field-id (if v 1 0) (if default 1 0) (lambda (b x) (builder-prepend-u8 b x))))
(define (builder-add-u8-slot b field-id v [default 0]) (add-scalar-slot b field-id v default builder-prepend-u8))
(define (builder-add-u16-slot b field-id v [default 0]) (add-scalar-slot b field-id v default builder-prepend-u16))
(define (builder-add-u32-slot b field-id v [default 0]) (add-scalar-slot b field-id v default builder-prepend-u32))
(define (builder-add-u64-slot b field-id v [default 0]) (add-scalar-slot b field-id v default builder-prepend-u64))
(define (builder-add-i8-slot b field-id v [default 0]) (add-scalar-slot b field-id v default builder-prepend-i8))
(define (builder-add-i16-slot b field-id v [default 0]) (add-scalar-slot b field-id v default builder-prepend-i16))
(define (builder-add-i32-slot b field-id v [default 0]) (add-scalar-slot b field-id v default builder-prepend-i32))
(define (builder-add-i64-slot b field-id v [default 0]) (add-scalar-slot b field-id v default builder-prepend-i64))
(define (builder-add-f32-slot b field-id v [default 0.0]) (add-scalar-slot b field-id v default builder-prepend-f32))
(define (builder-add-f64-slot b field-id v [default 0.0]) (add-scalar-slot b field-id v default builder-prepend-f64))

;; Finishes the object: writes the soffset placeholder, the vtable (with
;; dedup), patches the soffset, and returns the table's offset.
(define (builder-end-object b)
  (check-mode! b 'object 'builder-end-object)
  (define start (builder-obj-start b))
  (define n (builder-obj-num-fields b))
  (define locs (builder-obj-fields b))
  ;; The table starts with its (placeholder) soffset field, 4-aligned.
  (builder-prep b 4)
  (prepend-bytes! b (integer->integer-bytes 0 4 #t #f))
  (define table-pos (builder-offset b))
  (define table-size (- table-pos start))
  ;; Vtable: [u16 vtable-size][u16 table-size][u16 offset per field], where a
  ;; field's entry is (table-pos - field-pos) in written positions, 0 if the
  ;; field is absent.
  (define entries (make-bytes (* 2 n)))
  (for ([id (in-range n)])
    (define field-pos (hash-ref locs id #f))
    (when field-pos
      (bytes-copy! entries (* 2 id)
                   (integer->integer-bytes (- table-pos field-pos) 2 #f #f))))
  (define vtable
    (bytes-append (integer->integer-bytes (* 2 (+ n 2)) 2 #f #f)
                  (integer->integer-bytes table-size 2 #f #f)
                  entries))
  (builder-prep b 2) ; vtable entries are u16; a no-op after the 4-aligned soffset
  (prepend-bytes! b vtable)
  (define vtable-pos (builder-offset b))
  ;; Vtable dedup: reuse an identical vtable written earlier.
  (define soffset
    (let ([existing (hash-ref (builder-vtables b) vtable #f)])
      (if existing
          (begin
            (set-builder-head! b (+ (builder-head b) (bytes-length vtable)))
            (- existing table-pos))
          (begin
            (hash-set! (builder-vtables b) vtable vtable-pos)
            (- vtable-pos table-pos)))))
  ;; Patch the soffset at the table start.  It may be negative when the
  ;; vtable lies after the table (dedup against an earlier table).
  (bytes-copy! (builder-buf b)
               (- (bytes-length (builder-buf b)) table-pos)
               (integer->integer-bytes soffset 4 #t #f))
  (set-builder-nested! b #f)
  (set-builder-obj-fields! b #f)
  table-pos)

;; Finishes the buffer: aligns the total length to `minalign` (so all
;; interior alignments hold), prepends the root uoffset, and returns the
;; finished byte string.  The result is a standard 32-bit,
;; non-size-prefixed FlatBuffers buffer.
(define (builder-finish b root)
  (when (builder-nested b)
    (raise-fb
     (format "flatbuffers: builder-finish called while ~a is under construction"
             (builder-nested b))))
  ;; root == offset is fine: the table then starts immediately after the
  ;; root uoffset (possible when the table's vtable was deduplicated).
  (unless (and (exact-nonnegative-integer? root) (<= 1 root (builder-offset b)))
    (raise-fb
     (format "flatbuffers: builder-finish: root offset ~a is not a valid object offset; did you call builder-end-object?"
             root)))
  (builder-prealign b 4 (builder-minalign b))
  (builder-prep b 4)
  ;; The root uoffset sits at address 0; its value is the distance from its
  ;; own address to the root table's address: total length - root, i.e.
  ;; (current offset after prep + 4) - root in written positions.
  (define value (- (+ (builder-offset b) 4) root))
  (check-uint value 32 'builder-finish)
  (prepend-bytes! b (integer->integer-bytes value 4 #f #f))
  (subbytes (builder-buf b) (builder-head b)))
