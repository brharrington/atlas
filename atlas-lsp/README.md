# Atlas LSP

Language Server Protocol implementation for the Atlas Stack Language.

## Usage

### Test Page

Run the built-in test server to experiment with LSP features in a browser:

```
project/sbt 'atlas-lsp/test:runMain com.netflix.atlas.lsp.AtlasLspRunner'
```

Then open http://localhost:7101. The test page provides a Monaco editor connected to
the LSP server over WebSocket with panels showing diagnostics, completions, cursor
token info, and an LSP message log.

### Editor Integration

The LSP server communicates over standard JSON-RPC. To use with an editor:

1. Build the LSP server jar
2. Launch it with stdin/stdout transport
3. Configure your editor's LSP client to connect

Currently supported capabilities:
- **Completions** — context-aware word suggestions filtered by stack state
- **Semantic tokens** — syntax highlighting for words, strings, numbers, parentheses, and comments
- **Diagnostics** — error and warning reporting for invalid expressions
- **Code actions** — expression formatting via refactor/rewrite

## TODO

### Code Actions
- [ ] Improve format action
- [ ] Add compress action
- [ ] Add normalize action
- [ ] Add rewrite action

### Completions
- [ ] Tag value completion
- [ ] Tag key completion
- [ ] Unicode character completion
- [ ] Improved operator completion

### Editor Features
- [ ] Hover documentation — show word summary and stack signature on hover
- [ ] Inlay hints — display intermediate stack state between tokens
- [ ] Signature help — show expected stack types when typing a word
- [ ] Go to definition — jump to variable definitions for `:set`/`:get`
- [ ] Expression preview — show evaluated result or final stack

### Other
- [ ] Glossary support
- [ ] Improve operation docs
- [ ] ASL vs URI LSP support
