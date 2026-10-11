# flatbuffers

Pure-Racket implementation of the [FlatBuffers](https://flatbuffers.dev/)
wire format (Google's zero-copy serialization):

- a **generic reader** — decodes any FlatBuffers buffer by numeric field
  id, with no schema codegen,
- a **low-level builder** — follows the reference builder's
  write-back-to-front model, with vtable deduplication and correct
  per-primitive alignment.

No FFI, no dependencies beyond `base`. Little-endian, 32-bit-offset,
non-size-prefixed buffers only — i.e. the core format produced/consumed by
current `flatc` releases. Schema codegen bindings are future work.

## Install

```
raco pkg install --auto --link flatbuffers        # from this directory
```

or, once published, `raco pkg install flatbuffers`.

## Quick start

```racket
(require flatbuffers)

(define b (make-builder))
(define s (builder-create-string b "hello"))
(builder-start-object b 2)
(builder-add-i32-slot b 0 42)
(builder-add-slot b 1 s)
(define root (builder-end-object b))
(define buf (builder-finish b root))

(define t (fb-root (open-flatbuffer buf)))
(table-field-i32 t 0)          ; => 42
(table-field-string t 1)       ; => "hello"
(table-field-i32 t 0 99)       ; absent fields return the default
```

Missing fields return the caller-supplied default (or the type default
`0` / `0.0` / `#f`) — FlatBuffers default elision, mirrored by the builder:
slot values `equal?` to their default are simply not written.

See `flatbuffers/scribblings/flatbuffers.scrbl` (or `raco docs
flatbuffers`) for the full API and a worked example around
`test/schema.fbs`.

## Testing

```
raco test -p flatbuffers
```

The suite always runs pure-Racket round trips (including an alignment fuzz
loop). If the `flatc` executable is on `PATH`, interop tests also run:
reader goldens from `flatc -b` binaries, writer validation via `flatc -t`
(compared field-by-field semantically — byte layout differences are legal),
and vtable-dedup acceptance. Without `flatc` (e.g. on Windows CI) the
interop part is skipped gracefully.

CI (`.github/workflows/ci.yml`) tests on ubuntu (flatc via
`flatbuffers-compiler`), macOS (flatc via Homebrew) and Windows
(round-trip only).

## License

MIT — see [LICENSE](LICENSE).
