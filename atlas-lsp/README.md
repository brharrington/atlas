# Atlas LSP

Language Server Protocol implementation for the Atlas Stack Language. Designed
to be used by both editors and AI tools that need deterministic context about
Atlas expressions and graph URIs.

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
- **Hover** — word summary, stack signature, and examples on hover
- **Go to definition** — jump from `:get` to corresponding `:set` definition
- **Diagnostics** — error and warning reporting for invalid expressions
- **Code actions** — expression formatting and compression via refactor/rewrite

## TODO

### Code Actions
- [ ] Improve format action — macros (`:stack`, `:area`, etc.) need TypedMacro with declared pop/push counts so the formatter can group them correctly with their arguments
- [x] Add compress action
- [ ] Add normalize action
- [ ] Add rewrite action

### Completions
- [ ] Tag value completion
- [ ] Tag key completion
- [ ] Unicode character completion
- [ ] Improved operator completion

### Editor Features
- [x] Hover documentation — show word summary and stack signature on hover
- [x] Go to definition — jump to variable definitions for `:set`/`:get`

### Expression Introspection (for AI tooling)
- [ ] Stack state inspection — return intermediate stack state at each step of evaluation
- [ ] Operator documentation — given an expression, return docs for each operator used
- [ ] Glossary lookup — connect metrics used in an expression to glossary docs when available
- [ ] Expression decomposition — break multi-expression queries into labeled constituent parts
- [ ] URI ↔ expression conversion — round-trip between graph URI form and raw expression form
- [ ] Structured validation — return errors with kind, expected/actual types, and fix suggestions
- [ ] Batch operations — accept multiple expressions per request for validation/formatting

### Other
- [ ] Glossary support
- [ ] Improve operation docs
- [ ] ASL vs URI LSP support
