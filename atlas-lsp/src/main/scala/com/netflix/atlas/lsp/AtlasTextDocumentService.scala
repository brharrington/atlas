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
package com.netflix.atlas.lsp

import java.util.concurrent.CompletableFuture
import java.util.concurrent.ConcurrentHashMap

import scala.jdk.CollectionConverters.*
import scala.util.Try

import com.netflix.atlas.core.stacklang.Interpreter
import org.eclipse.lsp4j.CompletionItem
import org.eclipse.lsp4j.CompletionItemKind
import org.eclipse.lsp4j.CompletionList
import org.eclipse.lsp4j.CompletionParams
import org.eclipse.lsp4j.DidChangeTextDocumentParams
import org.eclipse.lsp4j.DidCloseTextDocumentParams
import org.eclipse.lsp4j.DidOpenTextDocumentParams
import org.eclipse.lsp4j.DidSaveTextDocumentParams
import org.eclipse.lsp4j.jsonrpc.messages.Either as LspEither
import org.eclipse.lsp4j.services.TextDocumentService

class AtlasTextDocumentService(interpreter: Interpreter) extends TextDocumentService {

  private val documents = new ConcurrentHashMap[String, String]

  override def didOpen(params: DidOpenTextDocumentParams): Unit = {
    val doc = params.getTextDocument
    documents.put(doc.getUri, doc.getText)
  }

  override def didChange(params: DidChangeTextDocumentParams): Unit = {
    val changes = params.getContentChanges
    if (!changes.isEmpty) {
      documents.put(params.getTextDocument.getUri, changes.get(changes.size - 1).getText)
    }
  }

  override def didClose(params: DidCloseTextDocumentParams): Unit = {
    documents.remove(params.getTextDocument.getUri)
  }

  override def didSave(params: DidSaveTextDocumentParams): Unit = {}

  override def completion(
    params: CompletionParams
  ): CompletableFuture[LspEither[java.util.List[CompletionItem], CompletionList]] = {
    val uri = params.getTextDocument.getUri
    val text = documents.getOrDefault(uri, "")
    val offset = params.getPosition.getCharacter
    val items = computeCompletions(text, offset)
    val result = LspEither.forLeft[java.util.List[CompletionItem], CompletionList](items.asJava)
    CompletableFuture.completedFuture(result)
  }

  private[lsp] def computeCompletions(text: String, offset: Int): List[CompletionItem] = {
    // Split the expression at the cursor position and find the current token
    val beforeCursor = text.substring(0, math.min(offset, text.length))
    val tokens = Interpreter.splitAndTrim(beforeCursor)

    // Determine if the user is in the middle of typing a word (starts with ':')
    val (precedingTokens, currentPrefix) = tokens.lastOption match {
      case Some(tok) if tok.startsWith(":") =>
        // Cursor is on a partial word like ":eq" or ":"
        (tokens.init, tok.substring(1))
      case _ =>
        // Cursor is after a comma or at the start, complete with all words
        (tokens, "")
    }

    // Execute the preceding tokens to get the current stack state
    val stack = Try {
      interpreter.execute(precedingTokens.mkString(",")).stack
    }.getOrElse(Nil)

    // Find matching words: filter by prefix and by stack compatibility
    interpreter.vocabulary
      .filter(_.name.startsWith(currentPrefix))
      .filter(_.matches(stack))
      .distinctBy(_.name)
      .map { word =>
        val item = new CompletionItem(s":${word.name}")
        item.setKind(CompletionItemKind.Function)
        item.setDetail(word.signature)
        item.setDocumentation(word.summary)
        item
      }
  }
}