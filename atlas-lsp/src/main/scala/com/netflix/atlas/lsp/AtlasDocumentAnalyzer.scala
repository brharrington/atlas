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

import com.netflix.atlas.core.model.ExprNormalizer
import com.netflix.atlas.core.model.ModelDataTypes
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
import org.eclipse.lsp4j.DocumentSymbol
import org.eclipse.lsp4j.Hover
import org.eclipse.lsp4j.Location
import org.eclipse.lsp4j.MarkupContent
import org.eclipse.lsp4j.MarkupKind
import org.eclipse.lsp4j.Position
import org.eclipse.lsp4j.PublishDiagnosticsParams
import org.eclipse.lsp4j.Range
import org.eclipse.lsp4j.SymbolKind
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

  private val normalizer = new ExprNormalizer(
    com.typesafe.config.ConfigFactory.load().getConfig("atlas.core.normalize")
  )

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
    val paramDiags = computeParameterDiagnostics(tree)
    val allDiags = tree.diagnostics ++ paramDiags
    val diags = allDiags.map { d =>
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

  /**
    * Compute additional diagnostics that highlight specific stack items whose types
    * don't match the expected parameter types of a word. This helps users identify
    * which argument is wrong rather than just seeing "no matches" on the word itself.
    */
  private[lsp] def computeParameterDiagnostics(tree: SyntaxTree): List[Diagnostic] = {
    val flat = flattenNodes(tree.nodes)
    val diags = List.newBuilder[Diagnostic]

    flat.zipWithIndex.foreach {
      case (wn @ WordNode(_, _, stack, Some(d)), nodeIdx) if d.message.startsWith("no matches") =>
        val name = wn.token.value.stripPrefix(":")
        val candidates = interpreter.vocabulary.filter(_.name == name).collect {
          case tw: TypedWord => tw
        }
        bestCandidate(candidates, stack).foreach { tw =>
          val sourceMap = buildStackSourceMap(flat, nodeIdx)
          val params = tw.parameters
          val n = params.length
          // Parameters are in user-facing order (deepest first), stack has top first
          var i = 0
          while (i < n && i < stack.length) {
            val param = params(n - 1 - i)
            val value = stack(i)
            if (param.dataType.extract(value).isEmpty) {
              sourceMap.get(i).foreach { span =>
                val desc = param.dataType.description
                val expected =
                  if (desc.nonEmpty) s"${param.dataType.name} ($desc)"
                  else param.dataType.name
                val got = formatValueBrief(value)
                val msg = s"expected $expected, got $got"
                diags += Diagnostic(span, msg, Severity.Error)
              }
            }
            i += 1
          }
        }
      case _ =>
    }

    diags.result()
  }

  /** Find the candidate TypedWord whose parameters best match the given stack. */
  private def bestCandidate(candidates: List[TypedWord], stack: List[Any]): Option[TypedWord] = {
    if (candidates.isEmpty) return None
    candidates
      .filter(_.parameters.length <= stack.length)
      .maxByOption { tw =>
        val params = tw.parameters
        val n = params.length
        var count = 0
        var i = 0
        while (i < n) {
          if (params(n - 1 - i).dataType.extract(stack(i)).isDefined) count += 1
          i += 1
        }
        count
      }
  }

  /**
    * Build a mapping from stack position (0 = top) to the source node span that
    * produced the value at that position, relative to the error word at `errorIdx`.
    * Walk backwards through the flat node list, replaying stack effects.
    *
    * We maintain a virtual stack that tracks which items in the error word's stack
    * came from which node. `skip` counts items that need to be consumed by
    * intermediate words and should not be mapped.
    */
  private def buildStackSourceMap(
    flat: List[SyntaxNode],
    errorIdx: Int
  ): Map[Int, Span] = {
    val result = scala.collection.mutable.Map[Int, Span]()
    // pos = next error-stack position to assign; skip = items to skip (consumed
    // by intermediate words)
    var pos = 0
    var skip = 0
    var i = errorIdx - 1
    while (i >= 0) {
      flat(i) match {
        case ln: LiteralNode =>
          if (skip > 0) {
            skip -= 1
          } else {
            result(pos) = ln.span
            pos += 1
          }
        case wn: WordNode =>
          wn.word match {
            case Some(tw: TypedWord) =>
              val pushed = tw.outputs.length
              if (skip >= pushed) {
                // All outputs consumed by a later intermediate word
                skip -= pushed
                skip += tw.parameters.length
              } else {
                // Some or all outputs map to the error word's stack
                val skipped = skip
                skip = 0
                var j = skipped
                while (j < pushed) {
                  result(pos) = wn.span
                  pos += 1
                  j += 1
                }
                skip += tw.parameters.length
              }
            case _ =>
              // Opaque word — can't trace through it, stop
              return result.toMap
          }
        case _ => // comments, etc — skip
      }
      i -= 1
    }
    result.toMap
  }

  /** Format a stack value briefly for diagnostic messages. */
  private def formatValueBrief(value: Any): String = {
    value match {
      case s: String => s"""String "$s""""
      case n: Number => s"${n.getClass.getSimpleName} $n"
      case items: List[?] =>
        val inner = items.map(formatValueBrief).mkString(", ")
        s"List ($inner)"
      case other =>
        val typeName = other.getClass.getSimpleName
        val s = other.toString
        if (s.length > 40) s"$typeName ${s.take(37)}..." else s"$typeName $s"
    }
  }

  private[lsp] def computeTypoCodeActions(uri: String): List[Either[Command, CodeAction]] = {
    val text = getText(uri)
    if (text.isEmpty) return Nil
    val tree = interpreter.syntaxTree(text)
    val actions = List.newBuilder[Either[Command, CodeAction]]
    tree.diagnostics.foreach { d =>
      val msg = d.message
      if (msg.startsWith("did you mean ':") && msg.endsWith("'? (semicolon instead of colon)")) {
        val name = msg.stripPrefix("did you mean ':").stripSuffix("'? (semicolon instead of colon)")
        val range = new Range(
          offsetToPosition(text, d.span.start),
          offsetToPosition(text, d.span.end)
        )
        val lspDiag = new org.eclipse.lsp4j.Diagnostic(
          range,
          d.message,
          DiagnosticSeverity.Warning,
          "atlas"
        )
        val edit = new TextEdit(range, s":$name")
        val wsEdit = new WorkspaceEdit(java.util.Map.of(uri, java.util.List.of(edit)))
        val action = new CodeAction(s"Replace with ':$name'")
        action.setKind(CodeActionKind.QuickFix)
        action.setDiagnostics(java.util.List.of(lspDiag))
        action.setEdit(wsEdit)
        actions += Either.forRight(action)
      }
    }
    actions.result()
  }

  private[lsp] def computeCodeActions(uri: String): List[Either[Command, CodeAction]] = {
    val text = getText(uri)
    if (text.isEmpty) return Nil
    val tree = interpreter.syntaxTree(text)
    val typoActions = computeTypoCodeActions(uri)
    val hasErrors = tree.diagnostics.exists(_.severity == Severity.Error)
    if (hasErrors) return typoActions
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

    try {
      val exprs = interpreter.execute(text).stack.collect {
        case ModelDataTypes.PresentationType(t) => t
      }
      if (exprs.nonEmpty) {
        val normalized = exprs.map(normalizer.normalizeToString).reverse.mkString(",")
        if (normalized != text) {
          val edit = new TextEdit(range, normalized)
          val wsEdit = new WorkspaceEdit(java.util.Map.of(uri, java.util.List.of(edit)))
          val action = new CodeAction("Normalize expression")
          action.setKind(CodeActionKind.RefactorRewrite)
          action.setEdit(wsEdit)
          actions += Either.forRight(action)
        }
      }
    } catch {
      case _: Exception => // Skip normalize action if execution fails
    }

    typoActions ++ actions.result()
  }

  private[lsp] def computeDocumentSymbols(text: String): List[DocumentSymbol] = {
    val tree = interpreter.syntaxTree(text)
    buildDocumentSymbols(text, tree.nodes)
  }

  private def buildDocumentSymbols(
    text: String,
    nodes: List[SyntaxNode]
  ): List[DocumentSymbol] = {
    import scala.collection.mutable
    val stack = mutable.ArrayBuffer[DocumentSymbol]()

    nodes.foreach {
      case node: LiteralNode =>
        val value = node.token.value
        val isNum = value.nonEmpty && (value.charAt(0).isDigit || value.charAt(0) == '-')
        val kind = if (isNum) SymbolKind.Number else SymbolKind.String
        val range = spanToRange(text, node.span)
        val sym = new DocumentSymbol(value, kind, range, range)
        stack += sym

      case _: CommentNode =>
      // skip comments

      case node: ListNode =>
        val children = buildDocumentSymbols(text, node.children)
        val range = spanToRange(text, node.span)
        val selRange = spanToRange(text, Span(node.open.span.start, node.open.span.end))
        val sym = new DocumentSymbol("(...)", SymbolKind.Array, range, selRange)
        sym.setChildren(children.asJava)
        stack += sym

      case node: WordNode =>
        val wordName =
          s":${node.word.map(_.name).getOrElse(node.token.value.stripPrefix(":"))}"
        val selRange = spanToRange(text, node.span)
        node.word match {
          case Some(tw: TypedWord) =>
            val popCount = tw.parameters.length
            val children = popSymbols(stack, popCount)
            val startPos =
              if (children.nonEmpty) children.head.getRange.getStart
              else selRange.getStart
            val range = new Range(startPos, selRange.getEnd)
            val sym = new DocumentSymbol(wordName, SymbolKind.Function, range, selRange)
            sym.setDetail(tw.signature)
            sym.setChildren(children.asJava)
            stack += sym
          case Some(w) =>
            w.name match {
              case "list" =>
                val children = stack.toList
                stack.clear()
                val startPos =
                  if (children.nonEmpty) children.head.getRange.getStart
                  else selRange.getStart
                val range = new Range(startPos, selRange.getEnd)
                val sym = new DocumentSymbol(wordName, SymbolKind.Function, range, selRange)
                sym.setDetail("(macro)")
                sym.setChildren(children.asJava)
                stack += sym
              case "each" | "map" =>
                val children = popSymbols(stack, 2)
                val startPos =
                  if (children.nonEmpty) children.head.getRange.getStart
                  else selRange.getStart
                val range = new Range(startPos, selRange.getEnd)
                val sym = new DocumentSymbol(wordName, SymbolKind.Function, range, selRange)
                sym.setDetail("(macro)")
                sym.setChildren(children.asJava)
                stack += sym
              case _ =>
                val sym = new DocumentSymbol(wordName, SymbolKind.Function, selRange, selRange)
                sym.setDetail("(macro)")
                stack += sym
            }
          case None =>
            val sym = new DocumentSymbol(wordName, SymbolKind.Function, selRange, selRange)
            sym.setDetail("unresolved")
            stack += sym
        }
    }
    stack.toList
  }

  /** Pop up to `count` symbols from the stack, returning them in original (bottom-up) order. */
  private def popSymbols(
    stack: scala.collection.mutable.ArrayBuffer[DocumentSymbol],
    count: Int
  ): List[DocumentSymbol] = {
    val n = math.min(count, stack.size)
    val result = stack.takeRight(n).toList
    stack.dropRightInPlace(n)
    result
  }

  private def spanToRange(text: String, span: Span): Range = {
    new Range(offsetToPosition(text, span.start), offsetToPosition(text, span.end))
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
  private case class CommandNode(args: List[FormatNode], text: String, size: Int) extends FormatNode

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
    val flat = flattenNodes(tree.nodes)
    findNodeAt(tree.nodes, offset).flatMap {
      case wn @ WordNode(_, Some(word), stack, _) =>
        val postStack = computePostStack(flat, tree.stack, wn)
        Some(wordHover(word, stack, postStack, text, offset))
      case _ => None
    }
  }

  /** Compute the post-execution stack for a word node by finding the next node's stack. */
  private def computePostStack(
    flat: List[SyntaxNode],
    treeStack: List[Any],
    node: WordNode
  ): List[Any] = {
    val idx = flat.indexOf(node)
    if (idx < 0) return treeStack
    // Find the next WordNode and use its pre-execution stack
    flat
      .drop(idx + 1)
      .collectFirst {
        case WordNode(_, _, stack, _) => stack
      }
      .getOrElse(treeStack)
  }

  private def wordHover(
    word: Word,
    preStack: List[Any],
    postStack: List[Any],
    text: String,
    offset: Int
  ): Hover = {
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
    appendStackSection(sb, word, preStack, postStack)
    val content = new MarkupContent(MarkupKind.MARKDOWN, sb.toString)
    val range = new Range(offsetToPosition(text, offset), offsetToPosition(text, offset))
    new Hover(content, range)
  }

  /** Append a Stack section showing the before → after transformation. */
  private def appendStackSection(
    sb: StringBuilder,
    word: Word,
    preStack: List[Any],
    postStack: List[Any]
  ): Unit = {
    val consumed = word match {
      case tw: TypedWord => preStack.take(tw.parameters.length)
      case _             => Nil
    }
    val produced = word match {
      case _: TypedWord =>
        // Items produced are those in postStack that weren't in the unconsumed portion
        val unconsumed = preStack.drop(consumed.length)
        val newCount = postStack.length - unconsumed.length
        if (newCount > 0) postStack.take(newCount) else Nil
      case _ => Nil
    }
    if (consumed.isEmpty && produced.isEmpty) return
    sb.append("\n\n**Stack:**\n```\n")
    val consumedStr = consumed.reverseIterator.map(formatStackItem).mkString(", ")
    val producedStr = produced.reverseIterator.map(formatStackItem).mkString(", ")
    if (consumedStr.nonEmpty && producedStr.nonEmpty)
      sb.append(s"$consumedStr \u2192 $producedStr")
    else if (consumedStr.nonEmpty)
      sb.append(s"$consumedStr \u2192 (empty)")
    else
      sb.append(s"(empty) \u2192 $producedStr")
    sb.append("\n```")
  }

  /** Format a stack item concisely for hover display. */
  private def formatStackItem(item: Any): String = {
    item match {
      case s: String if s.length > 40 => s"\"${s.take(37)}...\""
      case s: String                  => s"\"$s\""
      case n: Number                  => n.toString
      case items: List[?]             => items.map(formatStackItem).mkString("(", ", ", ")")
      case other =>
        val s = other.toString
        if (s.length > 60) s.take(57) + "..." else s
    }
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
          case _                                             => None
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
    flattenNodes(nodes).collect {
      case w: WordNode if w.span.end <= beforeOffset && setVarName(w).contains(name) =>
        w.span
    }.lastOption
  }

  private[lsp] def computeCompletions(text: String, offset: Int): List[CompletionItem] = {
    val beforeCursor = text.substring(0, math.min(offset, text.length))

    // Check if cursor is in a unicode escape sequence
    unicodePrefix(beforeCursor) match {
      case Some((prefix, backslashOffset)) =>
        val replaceStart = offsetToPosition(text, backslashOffset)
        val replaceEnd = offsetToPosition(text, offset)
        val replaceRange = new Range(replaceStart, replaceEnd)
        computeUnicodeCompletions(prefix, replaceRange)
      case None => computeWordCompletions(text, offset)
    }
  }

  /**
    * Extract the unicode prefix if the cursor is inside a `\uXXXX` or `\` sequence.
    * Returns the prefix (after `\u`) and the absolute offset of the backslash.
    */
  private def unicodePrefix(beforeCursor: String): Option[(String, Int)] = {
    val lastComma = beforeCursor.lastIndexOf(',')
    val tokenStart = lastComma + 1
    val token = beforeCursor.substring(tokenStart)
    val idx = token.lastIndexOf('\\')
    if (idx < 0) None
    else {
      val after = token.substring(idx + 1)
      val backslashOffset = tokenStart + idx
      // Match \, \u, or \uXXXX — skip the leading "u" if present
      if (after.isEmpty) Some(("", backslashOffset))
      else if (after.startsWith("u")) Some((after.substring(1), backslashOffset))
      else None
    }
  }

  private def computeWordCompletions(text: String, offset: Int): List[CompletionItem] = {
    val beforeCursor = text.substring(0, math.min(offset, text.length))
    val tree = interpreter.syntaxTree(beforeCursor)

    // Determine if the user is in the middle of typing a word or has completed one
    val lastWordNode = tree.nodes.reverseIterator.collectFirst { case w: WordNode => w }
    val (stack, currentPrefix, tokenStart) = lastWordNode match {
      case Some(w) if w.word.isDefined =>
        // Completed word that executed successfully — offer next-token completions
        (tree.stack, "", beforeCursor.length)
      case Some(w) =>
        // Partial or unknown word — prefix-filter using stack before this word
        (w.stack, w.token.value.stripPrefix(":"), w.span.start)
      case _ =>
        (tree.stack, "", beforeCursor.length)
    }

    // Add trailing comma unless the character after the cursor is already a comma
    val afterCursor = if (offset < text.length) text.charAt(offset) else '\u0000'
    val suffix = if (afterCursor == ',') "" else ","

    val replaceStart = offsetToPosition(text, tokenStart)
    val replaceEnd = offsetToPosition(text, offset)
    val replaceRange = new Range(replaceStart, replaceEnd)

    interpreter.vocabulary
      .filter(_.name.startsWith(currentPrefix))
      .filter {
        case tw: TypedWord => tw.matches(stack)
        case _             => true
      }
      .distinctBy(_.name)
      .map { word =>
        val item = new CompletionItem(s":${word.name}")
        item.setKind(CompletionItemKind.Function)
        item.setDetail(word.signature)
        item.setDocumentation(word.summary)
        item.setTextEdit(Either.forLeft(new TextEdit(replaceRange, s":${word.name}$suffix")))
        item
      }
  }

  /** Curated unicode characters commonly needed in ASL. */
  private val curatedUnicode: List[(Int, String)] = List(
    0x0020 -> "Space",
    0x0009 -> "Tab",
    0x000A -> "Newline",
    0x002C -> "Comma",
    0x003A -> "Colon",
    0x0028 -> "Left Parenthesis",
    0x0029 -> "Right Parenthesis",
    0x005C -> "Backslash"
  )

  private def computeUnicodeCompletions(
    prefix: String,
    replaceRange: Range
  ): List[CompletionItem] = {
    val lowerPrefix = prefix.toLowerCase
    val isHex = lowerPrefix.nonEmpty && lowerPrefix.forall("0123456789abcdef".contains(_))

    if (lowerPrefix.isEmpty) {
      // Just typed \u — show curated set
      curatedUnicode.map { case (cp, desc) => unicodeCompletionItem(cp, desc, replaceRange) }
    } else if (isHex) {
      // Hex prefix — filter curated by code, plus exact match if 4 digits
      val fromCurated = curatedUnicode.collect {
        case (cp, desc) if f"$cp%04x".startsWith(lowerPrefix) =>
          unicodeCompletionItem(cp, desc, replaceRange)
      }
      val exact = if (lowerPrefix.length == 4) {
        val cp = Integer.parseInt(lowerPrefix, 16)
        if (Character.isDefined(cp) && !curatedUnicode.exists(_._1 == cp)) {
          val name = Option(Character.getName(cp)).getOrElse("")
          List(unicodeCompletionItem(cp, name, replaceRange))
        } else Nil
      } else Nil
      fromCurated ++ exact
    } else {
      // Non-hex — search by character name across BMP
      val searchTerms = lowerPrefix.split("\\s+")
      val results = List.newBuilder[CompletionItem]
      var count = 0
      var cp = 0x20
      while (cp <= 0xFFFF && count < 50) {
        if (Character.isDefined(cp)) {
          val name = Character.getName(cp)
          if (name != null) {
            val lowerName = name.toLowerCase
            if (searchTerms.forall(lowerName.contains)) {
              results += unicodeCompletionItem(cp, name, replaceRange)
              count += 1
            }
          }
        }
        cp += 1
      }
      results.result()
    }
  }

  private def unicodeCompletionItem(
    codePoint: Int,
    description: String,
    replaceRange: Range
  ): CompletionItem = {
    val hex = f"$codePoint%04X"
    val ch = new String(Character.toChars(codePoint))
    val displayCh = if (codePoint < 0x21) "" else s"$ch "
    val item = new CompletionItem(s"$displayCh\\u$hex $description")
    item.setKind(CompletionItemKind.Text)
    item.setTextEdit(Either.forLeft(new TextEdit(replaceRange, s"\\u$hex")))
    item.setFilterText(s"\\u$hex $description")
    item.setDetail(s"U+$hex")
    item
  }

  /**
    * Compute semantic token data for the given expression. Returns the LSP-encoded
    * integer array: [deltaLine, deltaStart, length, tokenType, tokenModifiers] per token.
    */
  private[lsp] def computeSemanticTokens(text: String): List[Integer] = {
    val tree = interpreter.syntaxTree(text)
    val builder = List.newBuilder[Integer]
    // Build line start offset table for offset-to-line/col conversion
    val lineStarts = buildLineStarts(text)
    var prevLine = 0
    var prevCol = 0

    def encodeValueToken(token: ValueToken, tokenType: Int): Unit = {
      token.spans.foreach { s =>
        val (line, col) = offsetToLineCol(lineStarts, s.start)
        val deltaLine = line - prevLine
        val deltaStart = if (deltaLine == 0) col - prevCol else col
        val length = s.end - s.start
        builder += Integer.valueOf(deltaLine)
        builder += Integer.valueOf(deltaStart)
        builder += Integer.valueOf(length)
        builder += Integer.valueOf(tokenType)
        builder += Integer.valueOf(0) // no modifiers
        prevLine = line
        prevCol = col
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
          encodeValueToken(
            ValueToken(token.text, List(token.span)),
            AtlasTokenTypes.Comment
          )
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

  /** Build an array of line start offsets for the given text. */
  private def buildLineStarts(text: String): Array[Int] = {
    val starts = Array.newBuilder[Int]
    starts += 0
    var i = 0
    while (i < text.length) {
      if (text.charAt(i) == '\n') starts += (i + 1)
      i += 1
    }
    starts.result()
  }

  /** Convert an absolute offset to (line, col) using pre-computed line starts. */
  private def offsetToLineCol(lineStarts: Array[Int], offset: Int): (Int, Int) = {
    var lo = 0
    var hi = lineStarts.length - 1
    while (lo < hi) {
      val mid = (lo + hi + 1) / 2
      if (lineStarts(mid) <= offset) lo = mid else hi = mid - 1
    }
    (lo, offset - lineStarts(lo))
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
