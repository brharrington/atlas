import * as monaco from 'monaco-editor';
import editorWorker from 'monaco-editor/esm/vs/editor/editor.worker?worker';

// Monaco worker setup
self.MonacoEnvironment = {
  getWorker: () => new editorWorker(),
};

// Register the Atlas language (LSP provides all semantics)
monaco.languages.register({ id: 'atlas' });
monaco.languages.setLanguageConfiguration('atlas', {
  wordPattern: /[^\s,()]+/,
  comments: {
    blockComment: ['/*', '*/'],
  },
});

// Theme with semantic token colors
monaco.editor.defineTheme('atlas-dark', {
  base: 'vs-dark',
  inherit: true,
  rules: [
    { token: 'keyword',    foreground: 'DCDCAA', fontStyle: 'bold' },
    { token: 'operator',   foreground: 'DCDCAA', fontStyle: 'bold' },
    { token: 'function',   foreground: 'DCDCAA' },
    { token: 'string',     foreground: 'CE9178' },
    { token: 'number',     foreground: 'B5CEA8' },
    { token: 'comment',    foreground: '6A9955', fontStyle: 'italic' },
    { token: 'variable',   foreground: '9CDCFE' },
    { token: 'type',       foreground: '4EC5D4' },
    { token: 'parameter',  foreground: 'D4D4D4' },
  ],
  colors: {
    'editor.background': '#1e1e1e',
  },
  semanticHighlighting: true,
});

const SAMPLE = `name,sps,:eq,:sum,
name,sps,:eq,errors,:sum,
:div,
100,:mul`;

// Create editor
const editor = monaco.editor.create(document.getElementById('editor'), {
  value: SAMPLE,
  language: 'atlas',
  theme: 'atlas-dark',
  'semanticHighlighting.enabled': true,
  fontSize: 14,
  minimap: { enabled: false },
  scrollBeyondLastLine: false,
  renderLineHighlight: 'none',
  padding: { top: 8, bottom: 8 },
  automaticLayout: true,
  fixedOverflowWidgets: true,
  wordBasedSuggestions: 'off',
});

// Connect to Atlas LSP server
const dot = document.getElementById('status-dot');
const text = document.getElementById('status-text');

function connect() {
  const ws = new WebSocket('ws://localhost:7102');
  ws.onopen = () => {
    const transport = monaco.lsp.WebSocketTransport.fromWebSocket(ws);
    new monaco.lsp.MonacoLspClient(transport);
    dot.className = 'dot connected';
    text.textContent = 'LSP: connected';
  };
  ws.onerror = () => {
    dot.className = 'dot disconnected';
    text.textContent = 'LSP: connection failed';
  };
  ws.onclose = () => {
    dot.className = 'dot disconnected';
    text.textContent = 'LSP: disconnected — reconnecting...';
    setTimeout(connect, 3000);
  };
}

connect();
