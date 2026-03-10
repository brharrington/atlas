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
import org.eclipse.lsp4j.CodeAction
import org.eclipse.lsp4j.CodeActionKind
import org.eclipse.lsp4j.Command
import org.eclipse.lsp4j.CompletionItem
import org.eclipse.lsp4j.CompletionItemKind
import org.eclipse.lsp4j.DiagnosticSeverity
import org.eclipse.lsp4j.Position
import org.eclipse.lsp4j.PublishDiagnosticsParams
import org.eclipse.lsp4j.Range
import org.eclipse.lsp4j.TextEdit
import org.eclipse.lsp4j.WorkspaceEdit
import org.eclipse.lsp4j.jsonrpc.messages.Either
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

  private[lsp] def computeCodeActions(uri: String): List[Either[Command, CodeAction]] = {
    val text = getText(uri)
    if (text.isEmpty) return Nil
    val tree = interpreter.syntaxTree(text)
    val hasErrors = tree.diagnostics.exists(_.severity == Severity.Error)
    if (hasErrors) return Nil
    val formatted = formatExpression(text, tree.nodes)
    if (formatted == text) Nil
    else {
      val range = new Range(new Position(0, 0), offsetToPosition(text, text.length))
      val edit = new TextEdit(range, formatted)
      val wsEdit = new WorkspaceEdit(java.util.Map.of(uri, java.util.List.of(edit)))
      val action = new CodeAction("Format expression")
      action.setKind(CodeActionKind.RefactorRewrite)
      action.setEdit(wsEdit)
      List(Either.forRight(action))
    }
  }

  // --- Expression formatter ---

  /** Node in the formatting tree. */
  private sealed trait FormatNode {

    /** Number of stack slots this node occupies. */
    def size: Int
  }

  /** A literal value, comment, or unresolved word. */
  private case class SimpleNode(text: String) extends FormatNode {
    def size: Int = 1
  }

  /** A command that consumed arguments from the stack. */
  private case class CommandNode(args: List[FormatNode], text: String, size: Int)
      extends FormatNode

  /** A parenthesized list. */
  private case class ParenNode(children: List[FormatNode]) extends FormatNode {
    def size: Int = 1
  }

  /** Format an expression by building a tree and rendering with line breaks. */
  private[lsp] def formatExpression(expr: String, nodes: List[SyntaxNode]): String = {
    val tree = buildFormatTree(expr, nodes)
    renderFormatTree(tree)
  }

  /** Extract the original text for a syntax node from the expression string. */
  private def nodeText(expr: String, node: SyntaxNode): String = {
    expr.substring(node.span.start, node.span.end)
  }

  /**
    * Build a format tree from syntax nodes. Each command groups with its arguments
    * based on pop/push counts from the resolved TypedWord.
    */
  private def buildFormatTree(
    expr: String,
    nodes: List[SyntaxNode]
  ): List[FormatNode] = {
    import scala.collection.mutable
    val stack = mutable.ArrayBuffer[FormatNode]()

    nodes.foreach {
      case node: LiteralNode =>
        stack += SimpleNode(nodeText(expr, node))

      case node: CommentNode =>
        stack += SimpleNode(nodeText(expr, node))

      case node: ListNode =>
        val children = buildFormatTree(expr, node.children)
        stack += ParenNode(children)

      case node: WordNode =>
        val wordName = s":${node.word.map(_.name).getOrElse(node.token.value.stripPrefix(":"))}"
        node.word match {
          case Some(tw: TypedWord) =>
            popAndPush(stack, tw.parameters.length, tw.outputs.length, wordName)
          case Some(w) =>
            // Non-TypedWord words with known stack effects.
            // TODO: Replace with TypedMacro that declares pop/push counts.
            w.name match {
              case "list" =>
                val args = stack.toList
                stack.clear()
                stack += CommandNode(args, wordName, 1)
              case "each" | "map" =>
                popAndPush(stack, 2, 1, wordName)
              case _ =>
                stack += SimpleNode(wordName)
            }
          case None =>
            stack += SimpleNode(wordName)
        }
    }
    stack.toList
  }

  /** Pop `popCount` stack slots and push a CommandNode with the given push count. */
  private def popAndPush(
    stack: scala.collection.mutable.ArrayBuffer[FormatNode],
    popCount: Int,
    pushCount: Int,
    wordName: String
  ): Unit = {
    var remaining = popCount
    val args = List.newBuilder[FormatNode]
    while (remaining > 0 && stack.nonEmpty) {
      val top = stack.remove(stack.size - 1)
      remaining -= top.size
      args += top
    }
    stack += CommandNode(args.result().reverse, wordName, pushCount)
  }

  /** Check if a format node contains nested commands (is "complex"). */
  private def isComplex(node: FormatNode): Boolean = node match {
    case _: CommandNode => true
    case _              => false
  }

  /** Words where the preceding separator should stay inline (no newline before). */
  private val inlineWords: Set[String] = Set(":and", ":or", ":not")

  /** Render a format tree to a formatted string with line breaks. */
  private def renderFormatTree(nodes: List[FormatNode]): String = {
    nodes.map(n => renderNode(n, indent = false)).mkString(",\n\n")
  }

  /** Max line length before a list is broken across multiple lines. */
  private val maxLineLength = 78

  private def renderNode(node: FormatNode, indent: Boolean): String = node match {
    case SimpleNode(text) => text
    case ParenNode(children) =>
      val rendered = children.map(n => renderNode(n, indent = false))
      val inline = s"(,${rendered.mkString(",")},)"
      if (inline.length <= maxLineLength) inline
      else s"(\n  ${rendered.mkString(",\n  ")}\n)"
    case CommandNode(args, text, _) =>
      if (args.isEmpty) {
        text
      } else {
        val allSimple = args.forall(!isComplex(_))
        val indentChildren = indent || text == ":set"
        val argSep =
          if (text == ":list") ",\n\n"
          else if (allSimple) ","
          else if (indentChildren) ",\n  "
          else ",\n"
        val renderedArgs = args.map(a => renderNode(a, indentChildren))
        val lastArgSimple = !isComplex(args.last) || inlineWords.contains(text)
        val cmdSep =
          if (text == ":list" && !allSimple) ",\n\n"
          else if (lastArgSimple) ","
          else if (indent) ",\n  "
          else ",\n"
        renderedArgs.mkString(argSep) + cmdSep + text
      }
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
