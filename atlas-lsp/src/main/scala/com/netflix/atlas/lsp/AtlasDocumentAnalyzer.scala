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

import java.util.concurrent.ConcurrentHashMap

import scala.jdk.CollectionConverters.*

import com.netflix.atlas.core.stacklang.Interpreter
import com.netflix.atlas.core.stacklang.TypedWord
import com.netflix.atlas.core.stacklang.ast.*
import org.eclipse.lsp4j.CompletionItem
import org.eclipse.lsp4j.CompletionItemKind
import org.eclipse.lsp4j.DiagnosticSeverity
import org.eclipse.lsp4j.Position
import org.eclipse.lsp4j.PublishDiagnosticsParams
import org.eclipse.lsp4j.Range
import org.eclipse.lsp4j.services.LanguageClient

/**
  * Core analysis logic for the Atlas LSP. This class does NOT implement any LSP4j
  * service interfaces to avoid Scala/JDK annotation interop issues. The Java adapter
  * class [[AtlasTextDocumentService]] delegates to this class.
  */
class AtlasDocumentAnalyzer(
  val interpreter: Interpreter,
  clientSupplier: () => LanguageClient = () => null
) {

  private[lsp] val documents = new ConcurrentHashMap[String, String]

  def updateDocument(uri: String, text: String): Unit = {
    documents.put(uri, text)
    publishDiagnostics(uri, text)
  }

  def removeDocument(uri: String): Unit = {
    documents.remove(uri)
  }

  def getText(uri: String): String = {
    documents.getOrDefault(uri, "")
  }

  private[lsp] def publishDiagnostics(uri: String, text: String): Unit = {
    val client = clientSupplier()
    if (client == null) {
      System.err.println(s"DIAG: client is null, skipping diagnostics for: $text")
      return
    }
    val tree = interpreter.syntaxTree(text)
    val diags = tree.diagnostics.map { d =>
      val severity = d.severity match {
        case Severity.Error   => DiagnosticSeverity.Error
        case Severity.Warning => DiagnosticSeverity.Warning
        case Severity.Info    => DiagnosticSeverity.Information
      }
      val range = new Range(
        offsetToPosition(text, d.span.start),
        offsetToPosition(text, d.span.end)
      )
      new org.eclipse.lsp4j.Diagnostic(range, d.message, severity, "atlas")
    }
    System.err.println(s"DIAG: publishing ${diags.size} diagnostics for: $text")
    client.publishDiagnostics(new PublishDiagnosticsParams(uri, diags.asJava))
  }

  private[lsp] def computeCompletions(text: String, offset: Int): List[CompletionItem] = {
    val beforeCursor = text.substring(0, math.min(offset, text.length))
    val tree = interpreter.syntaxTree(beforeCursor)

    // Determine if the user is in the middle of typing a word or has completed one
    val lastWordNode = tree.nodes.reverseIterator.collectFirst { case w: WordNode => w }
    val (stack, currentPrefix) = lastWordNode match {
      case Some(w) if w.word.isDefined =>
        // Completed word that executed successfully — offer next-token completions
        (tree.stack, "")
      case Some(w) =>
        // Partial or unknown word — prefix-filter using stack before this word
        (w.stack, w.token.value.stripPrefix(":"))
      case _ =>
        (tree.stack, "")
    }

    interpreter.vocabulary
      .collect { case tw: TypedWord => tw }
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

  /**
    * Compute semantic token data for the given expression. Returns the LSP-encoded
    * integer array: [deltaLine, deltaStart, length, tokenType, tokenModifiers] per token.
    * All expressions are single-line, so deltaLine is always 0.
    */
  private[lsp] def computeSemanticTokens(text: String): List[Integer] = {
    val tree = interpreter.syntaxTree(text)
    val builder = List.newBuilder[Integer]
    var prevStart = 0

    def encodeValueToken(token: ValueToken, tokenType: Int): Unit = {
      token.spans.foreach { s =>
        appendToken(builder, s, tokenType, prevStart)
        prevStart = s.start
      }
    }

    def encodeNode(node: SyntaxNode): Unit = {
      node match {
        case LiteralNode(token) =>
          encodeValueToken(token, classifyLiteral(token.value))
        case WordNode(token, _, _, diagnostic) =>
          val tokenType = if (diagnostic.exists(_.severity == Severity.Error)) {
            AtlasTokenTypes.UnknownWord
          } else {
            AtlasTokenTypes.Word
          }
          encodeValueToken(token, tokenType)
        case ListNode(open, children, close, _) =>
          encodeValueToken(open, AtlasTokenTypes.Parenthesis)
          children.foreach(encodeNode)
          close.foreach(c => encodeValueToken(c, AtlasTokenTypes.Parenthesis))
        case CommentNode(token) =>
          appendToken(builder, token.span, AtlasTokenTypes.Comment, prevStart)
          prevStart = token.span.start
      }
    }

    tree.nodes.foreach(encodeNode)
    builder.result()
  }

  private def classifyLiteral(value: String): Int = {
    if (value.nonEmpty && (value.charAt(0).isDigit || value.charAt(0) == '-'))
      AtlasTokenTypes.Number
    else
      AtlasTokenTypes.String
  }

  private def appendToken(
    builder: collection.mutable.Builder[Integer, List[Integer]],
    span: Span,
    tokenType: Int,
    prevStart: Int
  ): Unit = {
    val deltaLine = 0 // single-line expressions
    val deltaStart = span.start - prevStart
    val length = span.end - span.start
    builder += Integer.valueOf(deltaLine)
    builder += Integer.valueOf(deltaStart)
    builder += Integer.valueOf(length)
    builder += Integer.valueOf(tokenType)
    builder += Integer.valueOf(0) // no modifiers
  }

  /** Convert an absolute character offset to an LSP Position (line, character). */
  private def offsetToPosition(text: String, offset: Int): Position = {
    var line = 0
    var col = 0
    var i = 0
    while (i < offset && i < text.length) {
      if (text.charAt(i) == '\n') {
        line += 1
        col = 0
      } else {
        col += 1
      }
      i += 1
    }
    new Position(line, col)
  }
}
