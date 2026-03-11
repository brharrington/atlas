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
import com.netflix.atlas.core.stacklang.Word
import org.eclipse.lsp4j.CodeAction
import org.eclipse.lsp4j.CodeActionKind
import org.eclipse.lsp4j.Command
import org.eclipse.lsp4j.CompletionItem
import org.eclipse.lsp4j.CompletionItemKind
import org.eclipse.lsp4j.DiagnosticSeverity
import org.eclipse.lsp4j.Hover
import org.eclipse.lsp4j.Location
import org.eclipse.lsp4j.MarkupContent
import org.eclipse.lsp4j.MarkupKind
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
    val actions = List.newBuilder[Either[Command, CodeAction]]
    val range = new Range(new Position(0, 0), offsetToPosition(text, text.length))

    val formatted = formatExpression(text, tree.nodes)
    if (formatted != text) {
      val edit = new TextEdit(range, formatted)
      val wsEdit = new WorkspaceEdit(java.util.Map.of(uri, java.util.List.of(edit)))
      val action = new CodeAction("Format expression")
      action.setKind(CodeActionKind.RefactorRewrite)
      action.setEdit(wsEdit)
      actions += Either.forRight(action)
    }

    val compressed = compressExpression(text, tree.nodes)
    if (compressed != text) {
      val edit = new TextEdit(range, compressed)
      val wsEdit = new WorkspaceEdit(java.util.Map.of(uri, java.util.List.of(edit)))
      val action = new CodeAction("Compress expression")
      action.setKind(CodeActionKind.RefactorRewrite)
      action.setEdit(wsEdit)
      actions += Either.forRight(action)
    }

    actions.result()
  }

  // --- Expression compressor ---

  /** Compress an expression by stripping whitespace, empty tokens, and line breaks. */
  private[lsp] def compressExpression(expr: String, nodes: List[SyntaxNode]): String = {
    val parts = List.newBuilder[String]
    compressNodes(expr, nodes, parts)
    parts.result().mkString(",")
  }

  private def compressNodes(
    expr: String,
    nodes: List[SyntaxNode],
    parts: collection.mutable.Builder[String, List[String]]
  ): Unit = {
    nodes.foreach {
      case LiteralNode(token) =>
        val v = token.value.trim
        if (v.nonEmpty) parts += v
      case WordNode(token, _, _, _) =>
        val v = token.value.trim
        if (v.nonEmpty) parts += v
      case CommentNode(token) =>
        parts += expr.substring(token.span.start, token.span.end)
      case ListNode(_, children, close, _) =>
        parts += "("
        compressNodes(expr, children, parts)
        if (close.isDefined) parts += ")"
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

  private[lsp] def computeHover(text: String, offset: Int): Option[Hover] = {
    val tree = interpreter.syntaxTree(text)
    findNodeAt(tree.nodes, offset).flatMap {
      case WordNode(_, Some(word), _, _) =>
        Some(wordHover(word, text, offset))
      case _ => None
    }
  }

  private def wordHover(word: Word, text: String, offset: Int): Hover = {
    val sb = new StringBuilder
    sb.append(s"**:${word.name}**\n\n")
    sb.append(s"`${word.signature}`\n\n")
    sb.append(word.summary)
    if (word.examples.nonEmpty) {
      sb.append("\n\n**Examples:**\n")
      word.examples.foreach { ex =>
        sb.append(s"\n- `$ex`")
      }
    }
    val content = new MarkupContent(MarkupKind.MARKDOWN, sb.toString)
    val range = new Range(offsetToPosition(text, offset), offsetToPosition(text, offset))
    new Hover(content, range)
  }

  /** Find the deepest syntax node whose span contains the given offset. */
  private def findNodeAt(nodes: List[SyntaxNode], offset: Int): Option[SyntaxNode] = {
    nodes.reverseIterator.collectFirst {
      case node if node.span.start <= offset && offset <= node.span.end =>
        node match {
          case ListNode(_, children, _, _) =>
            findNodeAt(children, offset).getOrElse(node)
          case _ => node
        }
    }
  }

  private[lsp] def computeDefinition(
    uri: String,
    text: String,
    offset: Int
  ): Option[Location] = {
    val tree = interpreter.syntaxTree(text)
    getVarNameAtCursor(tree.nodes, offset).flatMap { name =>
      findSetDefinition(tree.nodes, name, offset).map { span =>
        val range = new Range(offsetToPosition(text, span.start), offsetToPosition(text, span.end))
        new Location(uri, range)
      }
    }
  }

  /** Find the variable name at the cursor for :get references. */
  private def getVarNameAtCursor(
    nodes: List[SyntaxNode],
    offset: Int
  ): Option[String] = {
    val flat = flattenNodes(nodes)
    val idx = flat.indexWhere(n => n.span.start <= offset && offset <= n.span.end)
    if (idx < 0) return None

    flat(idx) match {
      case WordNode(_, Some(w), stack, _) if w.name == "get" =>
        // Cursor on :get — key name is top of pre-execution stack
        stack.headOption.collect { case s: String => s }
      case LiteralNode(token) if idx + 1 < flat.size =>
        // Cursor on literal — check if next node is :get
        flat(idx + 1) match {
          case WordNode(_, Some(w), _, _) if w.name == "get" => Some(token.value)
          case _ => None
        }
      case _ => None
    }
  }

  /** Flatten a syntax tree into a linear sequence of leaf nodes. */
  private def flattenNodes(nodes: List[SyntaxNode]): List[SyntaxNode] = {
    nodes.flatMap {
      case n: ListNode => flattenNodes(n.children)
      case n           => List(n)
    }
  }

  /** Extract the variable name being defined by a :set or :sset word. */
  private def setVarName(w: WordNode): Option[String] = w.word.flatMap { word =>
    word.name match {
      case "set"  => w.stack.lift(1).collect { case s: String => s }
      case "sset" => w.stack.headOption.collect { case s: String => s }
      case _      => None
    }
  }

  /** Find the most recent :set/:sset definition before the given offset. */
  private def findSetDefinition(
    nodes: List[SyntaxNode],
    name: String,
    beforeOffset: Int
  ): Option[Span] = {
    flattenNodes(nodes)
      .collect {
        case w: WordNode if w.span.end <= beforeOffset && setVarName(w).contains(name) =>
          w.span
      }
      .lastOption
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
