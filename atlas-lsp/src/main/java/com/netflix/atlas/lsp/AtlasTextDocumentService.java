/*
 * Copyright 2014-2026 Netflix, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.netflix.atlas.lsp;

import java.util.concurrent.CompletableFuture;

import org.eclipse.lsp4j.CompletionList;
import org.eclipse.lsp4j.CompletionItem;
import org.eclipse.lsp4j.CompletionParams;
import org.eclipse.lsp4j.DidChangeTextDocumentParams;
import org.eclipse.lsp4j.DidCloseTextDocumentParams;
import org.eclipse.lsp4j.DidOpenTextDocumentParams;
import org.eclipse.lsp4j.DidSaveTextDocumentParams;
import org.eclipse.lsp4j.SemanticTokens;
import org.eclipse.lsp4j.SemanticTokensParams;
import org.eclipse.lsp4j.jsonrpc.messages.Either;
import org.eclipse.lsp4j.services.TextDocumentService;

import scala.jdk.javaapi.CollectionConverters;

/**
 * Java adapter for {@link TextDocumentService} that delegates to {@link AtlasDocumentAnalyzer}.
 * Written in Java to avoid Scala/JDK annotation interop issues with LSP4j.
 */
public class AtlasTextDocumentService implements TextDocumentService {

    private final AtlasDocumentAnalyzer analyzer;

    public AtlasTextDocumentService(AtlasDocumentAnalyzer analyzer) {
        this.analyzer = analyzer;
    }

    AtlasDocumentAnalyzer analyzer() {
        return analyzer;
    }

    @Override
    public void didOpen(DidOpenTextDocumentParams params) {
        var doc = params.getTextDocument();
        analyzer.updateDocument(doc.getUri(), doc.getText());
    }

    @Override
    public void didChange(DidChangeTextDocumentParams params) {
        var changes = params.getContentChanges();
        if (!changes.isEmpty()) {
            var text = changes.get(changes.size() - 1).getText();
            var uri = params.getTextDocument().getUri();
            analyzer.updateDocument(uri, text);
        }
    }

    @Override
    public void didClose(DidCloseTextDocumentParams params) {
        analyzer.removeDocument(params.getTextDocument().getUri());
    }

    @Override
    public void didSave(DidSaveTextDocumentParams params) {}

    @Override
    public CompletableFuture<Either<java.util.List<CompletionItem>, CompletionList>> completion(
            CompletionParams params) {
        var uri = params.getTextDocument().getUri();
        var text = analyzer.getText(uri);
        var pos = params.getPosition();
        var offset = positionToOffset(text, pos.getLine(), pos.getCharacter());
        var items = analyzer.computeCompletions(text, offset);
        var javaItems = CollectionConverters.asJava(items);
        var result = Either.<java.util.List<CompletionItem>, CompletionList>forLeft(
                new java.util.ArrayList<>(javaItems));
        return CompletableFuture.completedFuture(result);
    }

    @Override
    public CompletableFuture<SemanticTokens> semanticTokensFull(SemanticTokensParams params) {
        var uri = params.getTextDocument().getUri();
        var text = analyzer.getText(uri);
        var data = analyzer.computeSemanticTokens(text);
        var javaData = CollectionConverters.asJava(data);
        return CompletableFuture.completedFuture(new SemanticTokens(new java.util.ArrayList<>(javaData)));
    }

    /** Convert an LSP line/character position to an absolute character offset. */
    private static int positionToOffset(String text, int line, int character) {
        int offset = 0;
        int currentLine = 0;
        while (currentLine < line && offset < text.length()) {
            if (text.charAt(offset) == '\n') {
                currentLine++;
            }
            offset++;
        }
        return offset + character;
    }
}