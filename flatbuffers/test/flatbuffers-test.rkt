#lang racket/base

;; Tests for the `flatbuffers` package.
;;
;; Layers:
;;
;;   1. Pure-Racket round trips: builder output read back by the generic
;;      reader, including an alignment fuzz loop (strings of length 1..40
;;      interleaved with uint64 fields).
;;
;;   2. flatc interop (skipped gracefully when `flatc` is not on PATH, e.g.
;;      on Windows CI):
;;        - reader goldens: binaries produced by `flatc -b` from JSON test
;;          data, including default elision cases;
;;        - writer validation: buffers built by our builder are converted
;;          back to JSON with `flatc -t` and compared field-by-field
;;          semantically (byte-for-byte equality is NOT required — the
;;          format leaves field order, object placement and vtable sharing
;;          unspecified; see https://flatbuffers.dev/internals/).

(require flatbuffers
         racket/file
         racket/list
         racket/path
         racket/runtime-path
         racket/system
         rackunit)

;; ---------------------------------------------------------------------------
;; Paths and helpers
;; ---------------------------------------------------------------------------

(define-runtime-path test-dir ".")
(define pkg-root (simplify-path (build-path test-dir ".." "..")))
(define schema-path (build-path pkg-root "test" "schema.fbs"))
(define data-dir (build-path pkg-root "test" "data"))

(define flatc-path (find-executable-path "flatc"))

(define (new-tmp-dir)
  ;; Temporary files live under the system temp directory, never in the repo.
  (make-temporary-file "racket-flatbuffers-test~a" 'directory))

(define (run-flatc! . args)
  (define stdout (open-output-string))
  (define stderr (open-output-string))
  (define code
    (parameterize ([current-output-port stdout]
                   [current-error-port stderr])
      (apply system*/exit-code flatc-path args)))
  (unless (zero? code)
    (error 'flatc "exit code ~a\n  args: ~s\n  stderr: ~a\n  stdout: ~a"
           code args (get-output-string stderr) (get-output-string stdout))))

;; `flatc -b` a JSON file into `out-dir`; returns the binary's path.
(define (flatc-json->bin! json-path out-dir)
  (run-flatc! "-b" "-o" (path->string out-dir)
              (path->string schema-path) (path->string json-path))
  (build-path out-dir
              (path-replace-extension (file-name-from-path json-path) ".bin")))

;; `flatc -t` a binary back to JSON in `out-dir`; returns the parsed jsexpr.
;; (Parsed with the minimal reader below because `racket/json` is not part
;; of `base` and this package's only dependency is `base`.)
(define (write-bytes! path bs)
  (display-to-file bs path #:exists 'replace))
(define (flatc-bin->json! bin-path out-dir #:root-type [root-type #f])
  (define args
    (append '("-t" "--raw-binary" "--strict-json")
            (if root-type (list "--root-type" root-type) '())
            (list "-o" (path->string out-dir)
                  (path->string schema-path)
                  "--"
                  (path->string bin-path))))
  (apply run-flatc! args)
  (define base (path->string (path-replace-extension (file-name-from-path bin-path) "")))
  (define candidates
    (list (build-path out-dir (string-append base ".json"))
          (build-path out-dir (string-append base ".bin.json"))))
  (define json-path
    (or (findf file-exists? candidates)
        (error 'flatc "no JSON output found for ~a (tried ~s)" bin-path candidates)))
  (parse-json (file->string json-path)))

;; ---------------------------------------------------------------------------
;; Minimal JSON reader (objects/arrays/strings/numbers/true/false/null).
;; flatc emits standard JSON; this keeps the package's dependency set at
;; exactly `base`.
;; ---------------------------------------------------------------------------

(define (parse-json str)
  (define in (open-input-string str))
  (define (skip-ws)
    (let loop ()
      (define c (peek-char in))
      (unless (eof-object? c)
        (when (char-whitespace? c)
          (read-char in)
          (loop)))))
  (define (expect c)
    (define got (read-char in))
    (unless (equal? got c)
      (error 'parse-json "expected ~s, got ~s" c got)))
  (define (expect-literal lit)
    (unless (equal? (read-string (string-length lit) in) lit)
      (error 'parse-json "expected literal ~s" lit)))
  (define (parse-unicode-escape)
    (define digits (read-string 4 in))
    (define cp (string->number digits 16))
    (unless cp (error 'parse-json "bad \\u escape: \\u~a" digits))
    (when (<= #xD800 cp #xDFFF)
      (error 'parse-json "surrogate escapes are not supported by the test JSON reader"))
    (integer->char cp))
  (define (parse-string)
    (expect #\")
    (let loop ([acc '()])
      (define c (read-char in))
      (cond
        [(eof-object? c) (error 'parse-json "unterminated string")]
        [(char=? c #\") (list->string (reverse acc))]
        [(char=? c #\\)
         (define e (read-char in))
         (define c*
           (case e
             [(#\") #\"]
             [(#\\) #\\]
             [(#\/) #\/]
             [(#\b) #\b]
             [(#\f) #\u000C]
             [(#\n) #\newline]
             [(#\r) #\return]
             [(#\t) #\tab]
             [(#\u) (parse-unicode-escape)]
             [else (error 'parse-json "bad escape: \\~a" e)]))
         (loop (cons c* acc))]
        [else (loop (cons c acc))])))
  (define (parse-number)
    (define chars
      (let loop ([acc '()])
        (define c (peek-char in))
        (if (and (char? c) (regexp-match? #rx"[-+.0-9eE]" (string c)))
            (begin (read-char in) (loop (cons c acc)))
            (reverse acc))))
    (define s (list->string chars))
    (define v (string->number s))
    (unless v (error 'parse-json "bad number: ~a" s))
    v)
  (define (parse-array)
    (expect #\[)
    (skip-ws)
    (if (equal? (peek-char in) #\])
        (begin (read-char in) '())
        (let loop ([acc '()])
          (skip-ws)
          (define v (parse-value))
          (skip-ws)
          (define c (read-char in))
          (case c
            [(#\,) (loop (cons v acc))]
            [(#\]) (reverse (cons v acc))]
            [else (error 'parse-json "expected , or ] in array, got ~s" c)]))))
  (define (parse-object)
    (expect #\{)
    (skip-ws)
    (if (equal? (peek-char in) #\})
        (begin (read-char in) (hash))
        (let loop ([acc '()])
          (skip-ws)
          (define key (string->symbol (parse-string)))
          (skip-ws)
          (expect #\:)
          (skip-ws)
          (define v (parse-value))
          (skip-ws)
          (define c (read-char in))
          (case c
            [(#\,) (loop (cons (cons key v) acc))]
            [(#\}) (make-immutable-hash (reverse (cons (cons key v) acc)))]
            [else (error 'parse-json "expected , or } in object, got ~s" c)]))))
  (define (parse-value)
    (skip-ws)
    (define c (peek-char in))
    (cond
      [(eof-object? c) (error 'parse-json "unexpected end of input")]
      [(char=? c #\{) (parse-object)]
      [(char=? c #\[) (parse-array)]
      [(char=? c #\") (parse-string)]
      [(char=? c #\t) (expect-literal "true") #t]
      [(char=? c #\f) (expect-literal "false") #f]
      [(char=? c #\n) (expect-literal "null") 'null]
      [else (parse-number)]))
  (begin0 (parse-value)
    (skip-ws)
    (unless (eof-object? (peek-char in))
      (error 'parse-json "trailing garbage after JSON value"))))

;; ---------------------------------------------------------------------------
;; Shared checkers
;; ---------------------------------------------------------------------------

(define (u8-vector->list v)
  (for/list ([i (in-range (fb-vector-length v))])
    (fb-vector-ref-u8 v i)))

;; Every field of the Outer table as written by test/data/full.json.
(define (check-outer-full! t)
  (check-equal? (table-field-string t 0) "hello")
  ;; nested table
  (define inner (table-field-table t 1))
  (check-pred table? inner)
  (check-equal? (table-field-i32 inner 0) 1)
  (check-equal? (table-field-i32 inner 1) -2)
  (check-equal? (table-field-string inner 2) "in")
  ;; vector of tables; inners[1] omits y and name
  (define inners (table-field-vector t 2))
  (check-pred fb-vector? inners)
  (check-equal? (fb-vector-length inners) 2)
  (define i0 (fb-vector-ref-table inners 0))
  (check-equal? (table-field-i32 i0 0) 10)
  (check-equal? (table-field-i32 i0 1) 20)
  (check-equal? (table-field-string i0 2) "a")
  (define i1 (fb-vector-ref-table inners 1))
  (check-equal? (table-field-i32 i1 0) -5)
  (check-equal? (table-field-i32 i1 1) 0)     ; omitted -> schema default
  (check-equal? (table-field-string i1 2) "b")
  ;; vector of strings
  (define tags (table-field-vector t 3))
  (check-equal? (fb-vector-length tags) 2)
  (check-equal? (fb-vector-ref-string tags 0) "alpha")
  (check-equal? (fb-vector-ref-string tags 1) "beta")
  ;; every scalar type
  (define sc (table-field-table t 4))
  (check-pred table? sc)
  (check-true (table-field-bool sc 0))
  (check-equal? (table-field-i8 sc 1) -8)
  (check-equal? (table-field-u8 sc 2) 200)
  (check-equal? (table-field-i16 sc 3) -300)
  (check-equal? (table-field-u16 sc 4) 5000)
  (check-equal? (table-field-i32 sc 5) -70000)
  (check-equal? (table-field-u32 sc 6) 3000000000)
  (check-equal? (table-field-i64 sc 7) -5000000000)
  (check-equal? (table-field-u64 sc 8) 10000000000)
  (check-equal? (table-field-f32 sc 9) 1.5)
  (check-equal? (table-field-f64 sc 10) -2.25)
  (check-equal? (table-field-string sc 11) "scalar-string")
  (check-equal? (u8-vector->list (table-field-vector sc 12)) '(1 2 3 255))
  ;; presence
  (check-true (table-field-present? t 3))
  (check-true (table-field-present? sc 5)))

;; Builds the writer-test buffer: an Outer with the same field ids as
;; test/schema.fbs.  Both `inners` entries have identical shape so the
;; builder must deduplicate their vtable.
(define (build-writer-buffer!)
  (define b (make-builder))
  ;; strings and vectors first (they are referenced by uoffsets)
  (define label-off (builder-create-string b "writer-test"))
  (define inner-name-off (builder-create-string b "nested"))
  (define one-off (builder-create-string b "one"))
  (define two-off (builder-create-string b "two"))
  (define alpha-off (builder-create-string b "alpha"))
  (define beta-off (builder-create-string b "beta"))
  (define s-off (builder-create-string b "str"))
  (define data-off (builder-create-u8-vector b (bytes 1 2 3 255)))
  ;; scalars table (field ids 0..12)
  (builder-start-object b 13)
  (builder-add-bool-slot b 0 #t)
  (builder-add-i8-slot b 1 -8)
  (builder-add-u8-slot b 2 200)
  (builder-add-i16-slot b 3 -300)
  (builder-add-u16-slot b 4 5000)
  (builder-add-i32-slot b 5 -70000)
  (builder-add-u32-slot b 6 3000000000)
  (builder-add-i64-slot b 7 -5000000000)
  (builder-add-u64-slot b 8 10000000000)
  (builder-add-f32-slot b 9 1.5)
  (builder-add-f64-slot b 10 -2.25)
  (builder-add-slot b 11 s-off)
  (builder-add-slot b 12 data-off)
  (define scalars-off (builder-end-object b))
  ;; inner table
  (builder-start-object b 3)
  (builder-add-i32-slot b 0 42)
  (builder-add-i32-slot b 1 -7)
  (builder-add-slot b 2 inner-name-off)
  (define inner-off (builder-end-object b))
  ;; inners vector: prepend elements in reverse order
  (builder-start-object b 3)
  (builder-add-i32-slot b 0 1)
  (builder-add-slot b 2 one-off)
  (define inner-a-off (builder-end-object b))
  (builder-start-object b 3)
  (builder-add-i32-slot b 0 2)
  (builder-add-slot b 2 two-off)
  (define inner-b-off (builder-end-object b))
  (builder-start-vector b 2 4)          ; 2 elements, uoffset (4 bytes) each
  (builder-prepend-uoffset b inner-b-off)
  (builder-prepend-uoffset b inner-a-off)
  (define inners-off (builder-end-vector b 2))
  ;; tags vector of strings
  (builder-start-vector b 2 4)
  (builder-prepend-uoffset b beta-off)
  (builder-prepend-uoffset b alpha-off)
  (define tags-off (builder-end-vector b 2))
  ;; root Outer table (field ids 0..4)
  (builder-start-object b 5)
  (builder-add-slot b 0 label-off)
  (builder-add-slot b 1 inner-off)
  (builder-add-slot b 2 inners-off)
  (builder-add-slot b 3 tags-off)
  (builder-add-slot b 4 scalars-off)
  (define root (builder-end-object b))
  (builder-finish b root))

(define (check-writer-roundtrip! buf)
  (define f (open-flatbuffer buf))
  (define t (fb-root f))
  (check-equal? (table-field-string t 0) "writer-test")
  (define inner (table-field-table t 1))
  (check-equal? (table-field-i32 inner 0) 42)
  (check-equal? (table-field-i32 inner 1) -7)
  (check-equal? (table-field-string inner 2) "nested")
  (define inners (table-field-vector t 2))
  (check-equal? (fb-vector-length inners) 2)
  (check-equal? (table-field-i32 (fb-vector-ref-table inners 0) 0) 1)
  (check-equal? (table-field-i32 (fb-vector-ref-table inners 1) 0) 2)
  (check-equal? (table-field-string (fb-vector-ref-table inners 1) 2) "two")
  (define tags (table-field-vector t 3))
  (check-equal? (fb-vector-ref-string tags 0) "alpha")
  (check-equal? (fb-vector-ref-string tags 1) "beta")
  (define sc (table-field-table t 4))
  (check-true (table-field-bool sc 0))
  (check-equal? (table-field-u64 sc 8) 10000000000)
  (check-equal? (table-field-f64 sc 10) -2.25)
  (check-equal? (table-field-string sc 11) "str")
  (check-equal? (u8-vector->list (table-field-vector sc 12)) '(1 2 3 255)))

;; ---------------------------------------------------------------------------
;; 1. Pure-Racket round trips
;; ---------------------------------------------------------------------------

(test-case "writer -> reader round trip (Outer table)"
  (define buf (build-writer-buffer!))
  (check-writer-roundtrip! buf))

(test-case "vtable dedup"
  ;; Both `inners` entries share a layout, so the second table must point
  ;; back at the first table's vtable.  In the finished buffer the earlier-
  ;; written vtable sits at a higher address, so the second table's soffset
  ;; is negative (vtable = table_pos - soffset still resolves it).
  (define buf (build-writer-buffer!))
  (define f (open-flatbuffer buf))
  (define t (fb-root f))
  (define inners (table-field-vector t 2))
  (define t1 (fb-vector-ref-table inners 0))
  (define t2 (fb-vector-ref-table inners 1))
  (define (table-soffset tab)
    (integer-bytes->integer
     (subbytes (fb-bytes (table-fb tab)) (table-pos tab) (+ (table-pos tab) 4))
     #t #f))
  (check-true (positive? (table-soffset t1)) "first table's vtable precedes it")
  (check-true (negative? (table-soffset t2)) "second table shares the first vtable")
  (check-equal? (table-field-i32 t2 0) 2)
  (check-equal? (table-field-string t2 2) "two"))

(test-case "empty string and empty byte vector"
  (define b (make-builder))
  (define empty-off (builder-create-string b ""))
  (define empty-vec (builder-create-u8-vector b #""))
  (builder-start-object b 2)
  (builder-add-slot b 0 empty-off)
  (builder-add-slot b 1 empty-vec)
  (define root (builder-end-object b))
  (define f (open-flatbuffer (builder-finish b root)))
  (define t (fb-root f))
  (check-equal? (table-field-string t 0) "")
  (check-equal? (fb-vector-length (table-field-vector t 1)) 0))

(test-case "alignment fuzz: strings of length 1..40 next to uint64 fields"
  ;; Varying string lengths shift every alignment boundary; the uint64
  ;; fields force non-trivial prep padding.  Catches padding bugs.
  (for ([len (in-range 1 41)])
    (define text (make-string len #\a))
    (define u64-a (arithmetic-shift 1 60))
    (define u64-b (* len 1000003))
    (define b (make-builder))
    (define s (builder-create-string b text))
    (builder-start-object b 3)
    (builder-add-u64-slot b 0 u64-a)
    (builder-add-u64-slot b 1 u64-b)
    (builder-add-slot b 2 s)
    (define root (builder-end-object b))
    (define f (open-flatbuffer (builder-finish b root)))
    (define t (fb-root f))
    (check-equal? (table-field-u64 t 0) u64-a (format "len=~a: field a" len))
    (check-equal? (table-field-u64 t 1) u64-b (format "len=~a: field b" len))
    (check-equal? (table-field-string t 2) text (format "len=~a: string" len))))

(test-case "buffer reuse across finishes"
  (define b (make-builder))
  (for ([n (in-range 1 4)])
    (define s (builder-create-string b (make-string n #\x)))
    (builder-start-object b 1)
    (builder-add-slot b 0 s)
    (define root (builder-end-object b))
    (define f (open-flatbuffer (builder-finish b root)))
    (check-equal? (table-field-string (fb-root f) 0) (make-string n #\x))))

;; ---------------------------------------------------------------------------
;; Error handling
;; ---------------------------------------------------------------------------

(test-case "reader errors on garbage"
  (check-exn exn:fail? (lambda () (open-flatbuffer #"ab")))
  (check-exn exn:fail? (lambda () (open-flatbuffer #"garbage!")))
  (check-exn exn:fail? (lambda () (open-flatbuffer (make-bytes 64 0))))
  (check-exn exn:fail? (lambda () (open-flatbuffer (make-bytes 1024 170))))
  (check-exn exn:fail? (lambda () (open-flatbuffer #"not-bytes"))))

(test-case "reader errors on out-of-range access"
  (define buf (build-writer-buffer!))
  (define f (open-flatbuffer buf))
  (define t (fb-root f))
  (check-exn exn:fail? (lambda () (table-field-i32 t -1)))
  ;; an offset vector read as scalars must not silently misalign out of the
  ;; buffer: reading past the end raises
  (define inners (table-field-vector t 2))
  (check-exn exn:fail? (lambda () (fb-vector-ref-table inners 99)))
  (check-exn exn:fail? (lambda () (fb-vector-ref-u8 inners 999999))))

(test-case "out-of-vtable field id behaves as absent"
  ;; Per the format: slots beyond the vtable count are absent fields.
  (define buf (build-writer-buffer!))
  (define f (open-flatbuffer buf))
  (define t (fb-root f))
  (check-false (table-field-present? t 100))
  (check-equal? (table-field-i32 t 100) 0)
  (check-equal? (table-field-i32 t 100 42) 42))

(test-case "builder misuse errors"
  (check-exn exn:fail?
             (lambda ()
               (define b (make-builder))
               (builder-start-object b 1)
               (builder-create-string b "no strings inside objects")))
  (check-exn exn:fail?
             (lambda ()
               (define b (make-builder))
               (builder-end-object b)))
  (check-exn exn:fail?
             (lambda ()
               (define b (make-builder))
               (builder-finish b 0)))
  (check-exn exn:fail?
             (lambda ()
               (define b (make-builder))
               (builder-start-object b 1)
               (builder-add-i32-slot b 5 1)))
  (check-exn exn:fail?
             (lambda ()
               (define b (make-builder))
               (builder-start-vector b 2 4)
               (builder-end-vector b 3)))
  (check-exn exn:fail?
             (lambda ()
               (define b (make-builder))
               (builder-start-object b 1)
               (builder-finish b 0)))
  (check-exn exn:fail?
             (lambda ()
               (define b (make-builder))
               (builder-prepend-u64 b -1))))

;; ---------------------------------------------------------------------------
;; 2. flatc interop (skipped when flatc is absent)
;; ---------------------------------------------------------------------------

(if flatc-path
    (begin
      (displayln "flatc found; running interop tests")

      (test-case "reader goldens from flatc-built binaries"
        (define tmp (new-tmp-dir))

        ;; full.json -> binary: every field type present
        (define full-bin (flatc-json->bin! (build-path data-dir "full.json") tmp))
        (check-outer-full! (fb-root (open-flatbuffer (file->bytes full-bin))))

        ;; minimal.json -> binary: fields explicitly set to their default
        ;; values are elided by flatc; the reader must return the defaults.
        (define min-bin (flatc-json->bin! (build-path data-dir "minimal.json") tmp))
        (define t-min (fb-root (open-flatbuffer (file->bytes min-bin))))
        (check-equal? (table-field-string t-min 0) "tiny")
        (check-true (table-field-present? t-min 4))
        (define sc (table-field-table t-min 4))
        (check-pred table? sc)
        (check-equal? (table-field-bool sc 0) #f)
        (check-equal? (table-field-i32 sc 5) 0)
        (check-equal? (table-field-f64 sc 10) 0.0)
        (check-equal? (table-field-u8 sc 2) 0)
        (check-false (table-field-string sc 11))
        (check-false (table-field-vector sc 12))
        ;; absent vector/table fields return the default (#f)
        (check-false (table-field-vector t-min 2))
        (check-false (table-field-table t-min 1))
        (check-false (table-field-present? t-min 2))

        ;; edge.json -> binary: empty string, empty vectors, defaults
        ;; elided inside a nested table, non-ASCII string
        (define edge-bin (flatc-json->bin! (build-path data-dir "edge.json") tmp))
        (define t-edge (fb-root (open-flatbuffer (file->bytes edge-bin))))
        (check-equal? (table-field-string t-edge 0) "")
        (check-equal? (fb-vector-length (table-field-vector t-edge 2)) 0)
        (check-equal? (fb-vector-length (table-field-vector t-edge 3)) 0)
        (define inner (table-field-table t-edge 1))
        (check-equal? (table-field-i32 inner 0) 0)   ; 0 in JSON -> elided
        (check-equal? (table-field-string inner 2) "zéro")
        (check-equal? (table-field-i32 inner 1) 0))

      (test-case "writer validation via flatc -t (semantic, not byte-for-byte)"
        (define tmp (new-tmp-dir))
        (define buf (build-writer-buffer!))
        (define bin-path (build-path tmp "writer.bin"))
        (write-bytes! bin-path buf)
        (define js (flatc-bin->json! bin-path tmp))
        ;; flatc parsed our buffer per test/schema.fbs; compare field values.
        (check-equal? (hash-ref js 'label) "writer-test")
        (define inner (hash-ref js 'inner))
        (check-equal? (hash-ref inner 'x) 42)
        (check-equal? (hash-ref inner 'y) -7)
        (check-equal? (hash-ref inner 'name) "nested")
        (define inners (hash-ref js 'inners))
        (check-equal? (length inners) 2)
        (check-equal? (hash-ref (list-ref inners 0) 'x) 1)
        (check-equal? (hash-ref (list-ref inners 1) 'x) 2)
        (check-equal? (hash-ref (list-ref inners 1) 'name) "two")
        (check-equal? (hash-ref js 'tags) '("alpha" "beta"))
        (define sc (hash-ref js 'scalars))
        (check-equal? (hash-ref sc 'b) #t)
        (check-equal? (hash-ref sc 'i8) -8)
        (check-equal? (hash-ref sc 'u8) 200)
        (check-equal? (hash-ref sc 'i16) -300)
        (check-equal? (hash-ref sc 'u16) 5000)
        (check-equal? (hash-ref sc 'i32) -70000)
        (check-equal? (hash-ref sc 'u32) 3000000000)
        (check-equal? (hash-ref sc 'i64) -5000000000)
        (check-equal? (hash-ref sc 'u64) 10000000000)
        (check-= (hash-ref sc 'f32) 1.5 1e-6)
        (check-= (hash-ref sc 'f64) -2.25 1e-9)
        (check-equal? (hash-ref sc 's) "str")
        (check-equal? (hash-ref sc 'data) '(1 2 3 255))
        ;; default elision on write: we never set inners[0].y (default 0),
        ;; so flatc must not report it either.
        (check-false (hash-has-key? (list-ref inners 0) 'y)))

      (test-case "flatc accepts our alignment padding"
        ;; A 13-byte string followed by uint64 fields forces padding that
        ;; depends on the total layout; flatc must decode the values.
        (define tmp (new-tmp-dir))
        (define text (make-string 13 #\a))
        (define b (make-builder))
        (define s (builder-create-string b text))
        (builder-start-object b 3)
        (builder-add-u64-slot b 0 (arithmetic-shift 1 60))
        (builder-add-u64-slot b 1 37)
        (builder-add-slot b 2 s)
        (define root (builder-end-object b))
        (define buf (builder-finish b root))
        (define bin-path (build-path tmp "aligned.bin"))
        (write-bytes! bin-path buf)
        (define js (flatc-bin->json! bin-path tmp #:root-type "Aligned"))
        (check-equal? (hash-ref js 'a) (arithmetic-shift 1 60))
        (check-equal? (hash-ref js 'b) 37)
        (check-equal? (hash-ref js 's) text))

      (test-case "round trip of flatc binary through our reader"
        ;; reader goldens re-read via the exact shared byte string; the
        ;; builder direction of the round trip is covered above.
        (define tmp (new-tmp-dir))
        (define full-bin (flatc-json->bin! (build-path data-dir "full.json") tmp))
        (check-outer-full! (fb-root (open-flatbuffer (file->bytes full-bin))))))
    (displayln "flatc not found; skipping interop tests (pure-Racket round-trip tests only)"))
