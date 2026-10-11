#lang racket/base

;; flatbuffers: a pure-Racket implementation of the FlatBuffers wire format.
;;
;; This module re-exports the generic reader and the low-level builder with
;; contracts.  See the package documentation (`raco docs flatbuffers`) for a
;; guided tour; the wire format implemented here is the core FlatBuffers
;; format as written by flatc: little-endian scalars and 32-bit offsets.
;; The 64-bit-offset variant is not supported in this version.

(require racket/contract
         flatbuffers/private/reader
         flatbuffers/private/builder)

(provide
 (contract-out
  ;; reader
  [struct fb ([bs bytes?])]
  [open-flatbuffer (-> bytes? fb?)]
  [fb-bytes (-> fb? bytes?)]
  [fb-root (-> fb? table?)]
  [struct table ([fb fb?] [pos exact-nonnegative-integer?])]
  [table-field-present? (-> table? exact-nonnegative-integer? boolean?)]
  [table-field-bool (->* (table? exact-nonnegative-integer?) (any/c) boolean?)]
  [table-field-u8 (->* (table? exact-nonnegative-integer?) (any/c) any/c)]
  [table-field-u16 (->* (table? exact-nonnegative-integer?) (any/c) any/c)]
  [table-field-u32 (->* (table? exact-nonnegative-integer?) (any/c) any/c)]
  [table-field-u64 (->* (table? exact-nonnegative-integer?) (any/c) any/c)]
  [table-field-i8 (->* (table? exact-nonnegative-integer?) (any/c) any/c)]
  [table-field-i16 (->* (table? exact-nonnegative-integer?) (any/c) any/c)]
  [table-field-i32 (->* (table? exact-nonnegative-integer?) (any/c) any/c)]
  [table-field-i64 (->* (table? exact-nonnegative-integer?) (any/c) any/c)]
  [table-field-f32 (->* (table? exact-nonnegative-integer?) (any/c) any/c)]
  [table-field-f64 (->* (table? exact-nonnegative-integer?) (any/c) any/c)]
  [table-field-string (->* (table? exact-nonnegative-integer?) (any/c) any/c)]
  [table-field-table (->* (table? exact-nonnegative-integer?) (any/c) any/c)]
  [table-field-vector
   (->* (table? exact-nonnegative-integer?)
        (any/c #:elem-size (or/c #f exact-positive-integer?))
        any/c)]
  [table-field-struct
   (->* (table? exact-nonnegative-integer? exact-positive-integer?) (any/c) any/c)]
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
  [fb-vector-ref-table (-> fb-vector? exact-nonnegative-integer? table?)]
  ;; builder
  [builder? (-> any/c boolean?)]
  [make-builder (->* () (exact-positive-integer?) builder?)]
  [builder-offset (-> builder? exact-nonnegative-integer?)]
  [builder-prep (-> builder? exact-positive-integer? void?)]
  [builder-prealign
   (-> builder? exact-nonnegative-integer? exact-positive-integer? void?)]
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
  [builder-start-vector
   (-> builder? exact-nonnegative-integer? exact-positive-integer? void?)]
  [builder-end-vector
   (-> builder? exact-nonnegative-integer? exact-nonnegative-integer?)]
  [builder-start-object (-> builder? exact-nonnegative-integer? void?)]
  [builder-end-object (-> builder? exact-nonnegative-integer?)]
  [builder-add-slot
   (-> builder? exact-nonnegative-integer? exact-nonnegative-integer? void?)]
  [builder-add-bool-slot
   (->* (builder? exact-nonnegative-integer? any/c) (any/c) void?)]
  [builder-add-u8-slot
   (->* (builder? exact-nonnegative-integer? (integer-in 0 255))
        ((integer-in 0 255)) void?)]
  [builder-add-u16-slot
   (->* (builder? exact-nonnegative-integer? (integer-in 0 65535))
        ((integer-in 0 65535)) void?)]
  [builder-add-u32-slot
   (->* (builder? exact-nonnegative-integer? exact-nonnegative-integer?)
        (exact-nonnegative-integer?) void?)]
  [builder-add-u64-slot
   (->* (builder? exact-nonnegative-integer? exact-nonnegative-integer?)
        (exact-nonnegative-integer?) void?)]
  [builder-add-i8-slot
   (->* (builder? exact-nonnegative-integer? (integer-in -128 127))
        ((integer-in -128 127)) void?)]
  [builder-add-i16-slot
   (->* (builder? exact-nonnegative-integer? (integer-in -32768 32767))
        ((integer-in -32768 32767)) void?)]
  [builder-add-i32-slot
   (->* (builder? exact-nonnegative-integer? exact-integer?)
        (exact-integer?) void?)]
  [builder-add-i64-slot
   (->* (builder? exact-nonnegative-integer? exact-integer?)
        (exact-integer?) void?)]
  [builder-add-f32-slot
   (->* (builder? exact-nonnegative-integer? real?) (real?) void?)]
  [builder-add-f64-slot
   (->* (builder? exact-nonnegative-integer? real?) (real?) void?)]
  [builder-finish (-> builder? exact-nonnegative-integer? bytes?)]))
