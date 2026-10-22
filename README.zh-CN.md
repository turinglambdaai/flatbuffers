# flatbuffers

[English](README.md) · **中文**

[![CI](https://github.com/turinglambdaai/flatbuffers/actions/workflows/ci.yml/badge.svg)](https://github.com/turinglambdaai/flatbuffers/actions/workflows/ci.yml) [![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)

[FlatBuffers](https://flatbuffers.dev/) 线格式的纯 Racket 实现（Google 的零拷贝序列化）：

- 一个**通用读取器**——按数字字段 id 解码任意 FlatBuffers 缓冲区，无需 schema 代码生成，
- 一个**底层构建器**——遵循参考实现的从后向前写入（write-back-to-front）模型，带 vtable 去重和正确的逐原语对齐。

无 FFI，除 `base` 外无任何依赖。仅支持小端序、32 位偏移、非 size-prefixed 的缓冲区——也就是当前 `flatc` 版本生产/消费的核心格式。Schema 代码生成绑定是后续工作。

## 安装

```
raco pkg install --auto --link flatbuffers        # from this directory
```

或者，发布之后可直接 `raco pkg install flatbuffers`。

## 快速上手

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

缺失字段返回调用方提供的默认值（或类型默认值 `0` / `0.0` / `#f`）——即 FlatBuffers 的默认值省略（default elision），构建器与之对称：与其默认值 `equal?` 的 slot 值不会被写入。

完整 API 与围绕 `test/schema.fbs` 的完整示例见 `flatbuffers/scribblings/flatbuffers.scrbl`（或 `raco docs flatbuffers`）。

## 测试

```
raco test -p flatbuffers
```

测试套件始终运行纯 Racket 的往返测试（包括一个对齐模糊测试循环）。若 `PATH` 上存在 `flatc` 可执行文件，还会运行互操作测试：以 `flatc -b` 生成的二进制作为读取端金标准、通过 `flatc -t` 验证写入结果（按字段逐个进行语义比较——字节布局差异是合法的），以及 vtable 去重的接受性测试。没有 `flatc` 时（例如 Windows CI 上）会优雅地跳过互操作部分。

CI（`.github/workflows/ci.yml`）在 ubuntu（flatc 来自 `flatbuffers-compiler`）、macOS（flatc 来自 Homebrew）和 Windows（仅往返测试）上运行测试。

## 许可证

MIT——见 [LICENSE](LICENSE)。
