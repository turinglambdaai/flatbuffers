# AGENTS.md

FlatBuffers 的纯 Racket 实现（MIT，已上架 Racket catalog）。

## 结构

Multi-collection 包：collection 即 `flatbuffers/` 子目录，scribblings 声明在 `flatbuffers/info.rkt`（collection 级）。`test/` 在包内。

## 常用命令

```bash
brew install flatbuffers        # 测试需要 flatc 编译器（CI 同款）
raco pkg install --auto --link "$(pwd)"
raco test -p flatbuffers
```

## 验证与发版

- CI（`ci.yml`）：装 flatc → 安装包 → `raco test -p flatbuffers`。
- 发版 = tag `v<版本>` + CHANGELOG 同步；catalog 侧为手动提交（pkgd）。
