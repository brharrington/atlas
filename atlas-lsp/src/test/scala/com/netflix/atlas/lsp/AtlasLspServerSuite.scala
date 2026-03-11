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

import scala.jdk.CollectionConverters.*

import com.netflix.atlas.core.model.StyleVocabulary
import com.netflix.atlas.core.stacklang.StandardVocabulary
import org.eclipse.lsp4j.CodeAction
import org.eclipse.lsp4j.CodeActionContext
import org.eclipse.lsp4j.CodeActionParams
import org.eclipse.lsp4j.Command
import org.eclipse.lsp4j.jsonrpc.messages.{Either => LspEither}
import org.eclipse.lsp4j.CompletionParams
import org.eclipse.lsp4j.DidOpenTextDocumentParams
import org.eclipse.lsp4j.InitializeParams
import org.eclipse.lsp4j.Position
import org.eclipse.lsp4j.Range
import org.eclipse.lsp4j.SemanticTokensParams
import org.eclipse.lsp4j.TextDocumentIdentifier
import org.eclipse.lsp4j.TextDocumentItem

import munit.FunSuite

class AtlasLspServerSuite extends FunSuite {

  private def newServer: AtlasLspServer = new AtlasLspServer(StandardVocabulary)

  private def openDocument(server: AtlasLspServer, uri: String, text: String): Unit = {
    val params = new DidOpenTextDocumentParams
    params.setTextDocument(new TextDocumentItem(uri, "atlas", 1, text))
    server.getTextDocumentService.didOpen(params)
  }

  private def requestCompletion(
    server: AtlasLspServer,
    uri: String,
    character: Int
  ): List[String] = {
    val params = new CompletionParams
    params.setTextDocument(new TextDocumentIdentifier(uri))
    params.setPosition(new Position(0, character))
    val result = server.getTextDocumentService.completion(params).get()
    result.getLeft.asScala.map(_.getLabel).toList
  }

  test("initialize enables completion") {
    val server = newServer
    val result = server.initialize(new InitializeParams).get()
    assertNotEquals(result.getCapabilities.getCompletionProvider, null)
  }

  test("shutdown completes successfully") {
    val server = newServer
    val result = server.shutdown().get()
    assertEquals(result, null)
  }

  test("completion: all words offered on empty expression") {
    val server = newServer
    val uri = "expr:1"
    openDocument(server, uri, "")
    val labels = requestCompletion(server, uri, 0)
    assert(labels.nonEmpty)
    assert(labels.contains(":depth"))
  }

  test("completion: words filtered by prefix") {
    val server = newServer
    val uri = "expr:2"
    openDocument(server, uri, "a,b,:sw")
    // cursor at end, on partial word ":sw"
    val labels = requestCompletion(server, uri, 7)
    assert(labels.contains(":swap"))
    assert(!labels.contains(":dup"))
  }

  test("completion: words filtered by stack") {
    val server = newServer
    val uri = "expr:3"
    // With two items on stack, :swap and :drop should match
    openDocument(server, uri, "a,b,")
    val labels = requestCompletion(server, uri, 4)
    assert(labels.contains(":swap"))
    assert(labels.contains(":drop"))
    assert(labels.contains(":dup"))
  }

  test("completion: items have detail and documentation") {
    val server = newServer
    val uri = "expr:4"
    openDocument(server, uri, "a,b,")
    val params = new CompletionParams
    params.setTextDocument(new TextDocumentIdentifier(uri))
    params.setPosition(new Position(0, 4))
    val items = server.getTextDocumentService.completion(params).get().getLeft.asScala
    val swap = items.find(_.getLabel == ":swap").get
    assertNotEquals(swap.getDetail, null)
    assertNotEquals(swap.getDocumentation, null)
  }

  //
  // semantic tokens
  //

  private def requestSemanticTokens(
    server: AtlasLspServer,
    uri: String
  ): List[Int] = {
    val params = new SemanticTokensParams
    params.setTextDocument(new TextDocumentIdentifier(uri))
    val result = server.getTextDocumentService.semanticTokensFull(params).get()
    result.getData.asScala.map(_.intValue()).toList
  }

  /** Decode the raw LSP integer array into (start, length, tokenType) tuples. */
  private def decodeTokens(data: List[Int]): List[(Int, Int, Int)] = {
    var pos = 0
    data
      .grouped(5)
      .map { group =>
        pos += group(1) // deltaStart (deltaLine is always 0)
        (pos, group(2), group(3)) // (absoluteStart, length, tokenType)
      }
      .toList
  }

  test("initialize enables semantic tokens") {
    val server = newServer
    val result = server.initialize(new InitializeParams).get()
    assertNotEquals(result.getCapabilities.getSemanticTokensProvider, null)
  }

  test("semantic tokens: literals classified as string") {
    val server = newServer
    val uri = "expr:st1"
    openDocument(server, uri, "abc")
    val tokens = decodeTokens(requestSemanticTokens(server, uri))
    assertEquals(tokens.size, 1)
    assertEquals(tokens.head, (0, 3, AtlasTokenTypes.String))
  }

  test("semantic tokens: numeric literal") {
    val server = newServer
    val uri = "expr:st2"
    openDocument(server, uri, "42")
    val tokens = decodeTokens(requestSemanticTokens(server, uri))
    assertEquals(tokens.size, 1)
    assertEquals(tokens.head, (0, 2, AtlasTokenTypes.Number))
  }

  test("semantic tokens: known word") {
    val server = newServer
    val uri = "expr:st3"
    openDocument(server, uri, "a,b,:swap")
    val tokens = decodeTokens(requestSemanticTokens(server, uri))
    assertEquals(tokens.size, 3)
    // "a" at 0, "b" at 2, ":swap" at 4
    assertEquals(tokens(0), (0, 1, AtlasTokenTypes.String))
    assertEquals(tokens(1), (2, 1, AtlasTokenTypes.String))
    assertEquals(tokens(2), (4, 5, AtlasTokenTypes.Word))
  }

  test("semantic tokens: unknown word") {
    val server = newServer
    val uri = "expr:st4"
    openDocument(server, uri, "a,:unknown")
    val tokens = decodeTokens(requestSemanticTokens(server, uri))
    assertEquals(tokens.size, 2)
    assertEquals(tokens(1)._3, AtlasTokenTypes.UnknownWord)
  }

  test("semantic tokens: list with parentheses") {
    val server = newServer
    val uri = "expr:st5"
    openDocument(server, uri, "(,a,b,)")
    val tokens = decodeTokens(requestSemanticTokens(server, uri))
    // ( at 0, a at 2, b at 4, ) at 6
    assertEquals(tokens.size, 4)
    assertEquals(tokens(0)._3, AtlasTokenTypes.Parenthesis)
    assertEquals(tokens(1)._3, AtlasTokenTypes.String)
    assertEquals(tokens(2)._3, AtlasTokenTypes.String)
    assertEquals(tokens(3)._3, AtlasTokenTypes.Parenthesis)
  }

  test("semantic tokens: mixed expression") {
    val server = newServer
    val uri = "expr:st6"
    openDocument(server, uri, "42,hello,:dup")
    val tokens = decodeTokens(requestSemanticTokens(server, uri))
    assertEquals(tokens.size, 3)
    assertEquals(tokens(0)._3, AtlasTokenTypes.Number)
    assertEquals(tokens(1)._3, AtlasTokenTypes.String)
    assertEquals(tokens(2)._3, AtlasTokenTypes.Word)
  }

  //
  // semantic tokens: comments
  //

  test("semantic tokens: standalone comment") {
    val server = newServer
    val uri = "expr:st7"
    openDocument(server, uri, "a,/* comment */,b")
    val tokens = decodeTokens(requestSemanticTokens(server, uri))
    val commentTokens = tokens.filter(_._3 == AtlasTokenTypes.Comment)
    assertEquals(commentTokens.size, 1)
    // "/* comment */" starts at 2, length 13
    assertEquals(commentTokens.head, (2, 13, AtlasTokenTypes.Comment))
  }

  test("semantic tokens: comment embedded in word") {
    val server = newServer
    val uri = "expr:st8"
    // a,:d/*c*/up -> word fragments :d at 2-4 and up at 9-11, comment /*c*/ at 4-9
    openDocument(server, uri, "a,:d/*c*/up")
    val tokens = decodeTokens(requestSemanticTokens(server, uri))
    // Expect: String "a" at 0, Word ":d" at 2, Comment "/*c*/" at 4, Word "up" at 9
    val wordTokens = tokens.filter(_._3 == AtlasTokenTypes.Word)
    val commentTokens = tokens.filter(_._3 == AtlasTokenTypes.Comment)
    assertEquals(wordTokens.size, 2) // two fragments of :dup
    assertEquals(commentTokens.size, 1)
    assertEquals(wordTokens(0), (2, 2, AtlasTokenTypes.Word)) // :d
    assertEquals(wordTokens(1), (9, 2, AtlasTokenTypes.Word)) // up
    assertEquals(commentTokens.head, (4, 5, AtlasTokenTypes.Comment)) // /*c*/
  }

  //
  // semantic tokens: multi-line
  //

  /** Decode raw LSP integer array into (line, col, length, tokenType) tuples. */
  private def decodeTokensMultiLine(data: List[Int]): List[(Int, Int, Int, Int)] = {
    var line = 0
    var col = 0
    data
      .grouped(5)
      .map { group =>
        line += group(0)
        col = if (group(0) > 0) group(1) else col + group(1)
        (line, col, group(2), group(3))
      }
      .toList
  }

  test("semantic tokens: multi-line expression") {
    val server = newServer
    val uri = "expr:st-ml1"
    // "a,\nb" — two lines with comma delimiter
    openDocument(server, uri, "a,\nb")
    val tokens = decodeTokensMultiLine(requestSemanticTokens(server, uri))
    assertEquals(tokens.size, 2)
    // "a" on line 0, col 0
    assertEquals(tokens(0), (0, 0, 1, AtlasTokenTypes.String))
    // "b" on line 1, col 0
    assertEquals(tokens(1), (1, 0, 1, AtlasTokenTypes.String))
  }

  test("semantic tokens: multi-line with word on second line") {
    val server = newServer
    val uri = "expr:st-ml2"
    // "a,b,\n:swap" — word on second line
    openDocument(server, uri, "a,b,\n:swap")
    val tokens = decodeTokensMultiLine(requestSemanticTokens(server, uri))
    val wordTokens = tokens.filter(_._4 == AtlasTokenTypes.Word)
    assertEquals(wordTokens.size, 1)
    // :swap on line 1, col 0
    assertEquals(wordTokens.head, (1, 0, 5, AtlasTokenTypes.Word))
  }

  //
  // code actions
  //

  private def requestCodeActions(
    server: AtlasLspServer,
    uri: String
  ): java.util.List[?] = {
    val text = server.analyzer().getText(uri)
    val params = new CodeActionParams
    params.setTextDocument(new TextDocumentIdentifier(uri))
    params.setRange(new Range(new Position(0, 0), new Position(0, text.length)))
    params.setContext(new CodeActionContext(java.util.List.of()))
    server.getTextDocumentService.codeAction(params).get()
  }

  test("initialize enables code actions") {
    val server = newServer
    val result = server.initialize(new InitializeParams).get()
    assertNotEquals(result.getCapabilities.getCodeActionProvider, null)
  }

  test("codeAction: no action for simple expression") {
    val server = newServer
    val uri = "expr:ca1"
    openDocument(server, uri, "a,b,:swap")
    val actions = requestCodeActions(server, uri)
    assertEquals(actions.size(), 0)
  }

  test("codeAction: no action on invalid expression") {
    val server = newServer
    val uri = "expr:ca2"
    openDocument(server, uri, ":unknown")
    val actions = requestCodeActions(server, uri)
    assertEquals(actions.size(), 0)
  }

  private def codeActionTitles(actions: java.util.List[?]): Seq[String] = {
    (0 until actions.size()).map { i =>
      val either = actions.get(i).asInstanceOf[LspEither[Command, CodeAction]]
      either.getRight.getTitle
    }
  }

  test("codeAction: normalize reorders query clauses") {
    val server = new AtlasLspServer(StyleVocabulary)
    val uri = "expr:ca-norm"
    openDocument(server, uri, "nf.cluster,foo,:eq,name,bar,:eq,:and,:sum")
    val actions = requestCodeActions(server, uri)
    val titles = codeActionTitles(actions)
    assert(titles.contains("Normalize expression"))
  }

  test("codeAction: normalize not offered when already normalized") {
    val server = new AtlasLspServer(StyleVocabulary)
    val uri = "expr:ca-norm2"
    openDocument(server, uri, "name,sps,:eq,:sum")
    val actions = requestCodeActions(server, uri)
    val titles = codeActionTitles(actions)
    assert(!titles.contains("Normalize expression"))
  }

  //
  // hover
  //

  private def requestHover(text: String, offset: Int): Option[org.eclipse.lsp4j.Hover] = {
    val server = newServer
    server.analyzer().computeHover(text, offset)
  }

  test("initialize enables hover") {
    val server = newServer
    val result = server.initialize(new InitializeParams).get()
    assertNotEquals(result.getCapabilities.getHoverProvider, null)
  }

  test("hover: word shows signature and summary") {
    val hover = requestHover("a,b,:swap", 5)
    assert(hover.isDefined)
    val content = hover.get.getContents.getRight.getValue
    assert(content.contains(":swap"), s"Expected word name in: $content")
    assert(content.contains("Swap"), s"Expected summary in: $content")
  }

  test("hover: literal returns None") {
    val hover = requestHover("a,b,:swap", 0)
    assert(hover.isEmpty)
  }

  test("hover: unknown word returns None") {
    val hover = requestHover(":unknown", 0)
    assert(hover.isEmpty)
  }

  private def requestModelHover(text: String, offset: Int): Option[org.eclipse.lsp4j.Hover] = {
    val server = new AtlasLspServer(StyleVocabulary)
    server.analyzer().computeHover(text, offset)
  }

  test("hover: :eq shows consumed args and result") {
    // name,sps,:eq
    // 0123456789012
    val hover = requestModelHover("name,sps,:eq", 9)
    assert(hover.isDefined)
    val content = hover.get.getContents.getRight.getValue
    assert(content.contains("Stack:"), s"Expected Stack section in: $content")
    assert(content.contains("\u2192"), s"Expected arrow in: $content")
    // Should show the consumed "name" and "sps" strings
    assert(content.contains("\"name\""), s"Expected consumed key in: $content")
    assert(content.contains("\"sps\""), s"Expected consumed value in: $content")
  }

  test("hover: :sum shows consumed Query and result") {
    // name,sps,:eq,:sum
    // 01234567890123456
    val hover = requestModelHover("name,sps,:eq,:sum", 13)
    assert(hover.isDefined)
    val content = hover.get.getContents.getRight.getValue
    assert(content.contains("Stack:"), s"Expected Stack section in: $content")
    assert(content.contains("\u2192"), s"Expected arrow in: $content")
  }

  test("hover: :swap shows stack items") {
    val hover = requestHover("a,b,:swap", 4)
    assert(hover.isDefined)
    val content = hover.get.getContents.getRight.getValue
    assert(content.contains("Stack:"), s"Expected Stack section in: $content")
    assert(content.contains("\"a\""), s"Expected stack item a in: $content")
    assert(content.contains("\"b\""), s"Expected stack item b in: $content")
  }

  test("hover: word with empty pre-stack shows produced items") {
    // :depth takes no parameters but produces an Int
    val hover = requestHover(":depth", 0)
    assert(hover.isDefined)
    val content = hover.get.getContents.getRight.getValue
    assert(content.contains("Stack:"), s"Expected Stack section in: $content")
    assert(content.contains("(empty) \u2192"), s"Expected empty input in: $content")
  }

  //
  // go to definition
  //

  private def requestDefinition(text: String, offset: Int): Option[org.eclipse.lsp4j.Location] = {
    val server = newServer
    server.analyzer().computeDefinition("test:uri", text, offset)
  }

  test("initialize enables definition") {
    val server = newServer
    val result = server.initialize(new InitializeParams).get()
    assertNotEquals(result.getCapabilities.getDefinitionProvider, null)
  }

  test("definition: :get jumps to :set") {
    // v,hello,:set,v,:get
    // 0123456789012345678
    val text = "v,hello,:set,v,:get"
    val defn = requestDefinition(text, 15) // cursor on :get
    assert(defn.isDefined)
    // :set span
    assertEquals(defn.get.getRange.getStart.getCharacter, 8)
    assertEquals(defn.get.getRange.getEnd.getCharacter, 12)
  }

  test("definition: variable name before :get jumps to :set") {
    val text = "v,hello,:set,v,:get"
    val defn = requestDefinition(text, 13) // cursor on "v" before :get
    assert(defn.isDefined)
    assertEquals(defn.get.getRange.getStart.getCharacter, 8)
  }

  test("definition: jumps to most recent :set") {
    // a,1,:set,a,2,:set,a,:get
    // 0123456789012345678901234
    val text = "a,1,:set,a,2,:set,a,:get"
    val defn = requestDefinition(text, 20) // cursor on :get
    assert(defn.isDefined)
    // Should jump to second :set at position 13, not first at position 4
    assertEquals(defn.get.getRange.getStart.getCharacter, 13)
  }

  test("definition: :sset support") {
    // 3,a,:sset,a,:get
    // 0123456789012345
    val text = "3,a,:sset,a,:get"
    val defn = requestDefinition(text, 12) // cursor on :get
    assert(defn.isDefined)
    assertEquals(defn.get.getRange.getStart.getCharacter, 4)
  }

  test("definition: no definition for literal without :get") {
    val text = "a,b,:swap"
    val defn = requestDefinition(text, 0) // cursor on "a"
    assert(defn.isEmpty)
  }

  //
  // compress expression
  //

  private def compress(text: String): String = {
    val server = newServer
    val tree = server.interpreter().syntaxTree(text)
    server.analyzer().compressExpression(text, tree.nodes)
  }

  test("compress: strips whitespace around tokens") {
    assertEquals(compress("a  ,  b  ,  :swap"), "a,b,:swap")
  }

  test("compress: removes empty tokens") {
    assertEquals(compress("a,b,,,:dup,"), "a,b,:dup")
  }

  test("compress: removes line breaks") {
    assertEquals(compress("a,\nb,\n:swap"), "a,b,:swap")
  }

  test("compress: preserves comments") {
    assertEquals(compress("a , /* hi */ , b"), "a,/* hi */,b")
  }

  test("compress: preserves lists") {
    assertEquals(compress("( , a , b , )"), "(,a,b,)")
  }

  test("compress: already compressed unchanged") {
    assertEquals(compress("a,b,:swap"), "a,b,:swap")
  }

  //
  // format expression (unit tests via analyzer)
  //

  private def format(text: String): String = {
    val server = newServer
    val tree = server.interpreter().syntaxTree(text)
    server.analyzer().formatExpression(text, tree.nodes)
  }

  /** Format using the full model vocabulary (query, data, math, style words). */
  private def formatModel(text: String): String = {
    val server = new AtlasLspServer(StyleVocabulary)
    val tree = server.interpreter().syntaxTree(text)
    server.analyzer().formatExpression(text, tree.nodes)
  }

  test("format: simple expression unchanged") {
    assertEquals(format("a,b,:swap"), "a,b,:swap")
  }

  test("format: single literal unchanged") {
    assertEquals(format("hello"), "hello")
  }

  test("format: nested operations get line breaks") {
    // :dup pops 1 pushes 2, :drop pops 1 pushes 0
    // tree: CommandNode([CommandNode([Simple("a")], ":dup", 2)], ":drop", 0)
    val input = "a,:dup,:drop"
    val expected = "a,:dup,\n:drop"
    assertEquals(format(input), expected)
  }

  test("format: multiple top-level items separated by blank line") {
    assertEquals(format("a,b"), "a,\n\nb")
  }

  test("format: model query with aggregation") {
    val input = "name,sps,:eq,:sum"
    val expected = "name,sps,:eq,\n:sum"
    assertEquals(formatModel(input), expected)
  }

  test("format: model group by with complex arg") {
    val input = "name,sps,:eq,:sum,(,name,),:by"
    val expected = "name,sps,:eq,\n:sum,\n(,name,),:by"
    assertEquals(formatModel(input), expected)
  }

  test("format: model multiple stack items") {
    val input = "name,sps,:eq,:sum,name,app,:eq,:sum"
    val expected = "name,sps,:eq,\n:sum,\n\nname,app,:eq,\n:sum"
    assertEquals(formatModel(input), expected)
  }

  test("format: model set indents value") {
    val input = "v,name,sps,:eq,:sum,:set"
    val result = formatModel(input)
    assert(result.contains("\n  "), s"Expected indented content in: $result")
  }

  test("format: model nested operations") {
    val input = "name,sps,:eq,:sum,(,name,),:by,4,:rolling-count,1,:gt,$name,:legend"
    val result = formatModel(input)
    assert(result.contains("\n"), s"Expected line breaks in: $result")
  }

  test("format: :list and :each grouped together") {
    val input = "name,a,:eq,:sum,:list,(,key,(,b,),:in,:cq,),:each"
    val result = formatModel(input)
    // :list and :each body should not have blank lines between them
    assert(
      !result.contains(":list,\n\n("),
      s"Unexpected blank line between :list and :each body in: $result"
    )
  }

  test("format: short list stays inline") {
    val input = "name,a,:eq,:sum,(,name,),:by"
    val result = formatModel(input)
    assert(result.contains("(,name,)"), s"Short list should be inline in: $result")
  }

  test("format: long list wraps to multiple lines") {
    val input =
      "name,a,:eq,:sum,(,us-west-2,us-east-1,us-east-2,eu-west-1,ap-southeast-1,ap-northeast-1,ap-southeast-2,),:by"
    val result = formatModel(input)
    assert(result.contains("\n  us-west-2"), s"Long list should be multi-line in: $result")
  }

  test("format: :list args separated by blank lines") {
    val input = "name,a,:eq,:sum,name,b,:eq,:sum,:list,(,key,(,c,),:in,:cq,),:each"
    val result = formatModel(input)
    // The two :sum expressions inside :list should be separated by blank lines
    assert(result.contains(":sum,\n\n"), s"Expected blank line between :list args in: $result")
    // :list and :each body should not have blank lines between them
    assert(
      !result.contains(":list,\n\n("),
      s"Unexpected blank line between :list and :each body in: $result"
    )
  }

  //
  // diagnostics: words inside lists
  //

  /** Get diagnostics from the syntax tree for the given expression using the model vocabulary. */
  private def modelDiagnostics(text: String): List[String] = {
    val server = new AtlasLspServer(StyleVocabulary)
    val tree = server.interpreter().syntaxTree(text)
    tree.diagnostics.map(d => s"[${d.span.start}-${d.span.end}] ${d.message}")
  }

  test("diagnostics: :each with :in in list body no false errors") {
    // When :each executes during syntax tree building, the list body may fail
    // because it runs against aggregate types rather than individual items.
    // The syntax tree should recognize the word matches and not emit a false
    // diagnostic.
    val input =
      "v,name,a,:eq,:sum,:set," +
        "v,:get,:list," +
        "(,key,(,b,),:in,:cq,),:each"
    val errors = modelDiagnostics(input).filter(_.contains("no matches"))
    assertEquals(errors, Nil, s"Unexpected errors: ${errors.mkString("; ")}")
  }

  //
  // unicode completions
  //

  test("completion: \\ prefix shows curated list") {
    val server = newServer
    val uri = "expr:uc0"
    openDocument(server, uri, "\\")
    val labels = requestCompletion(server, uri, 1)
    assert(labels.exists(_.contains("002C")), "should include comma")
    assert(labels.exists(_.contains("005C")), "should include backslash")
  }

  test("completion: \\u prefix shows curated list") {
    val server = newServer
    val uri = "expr:uc1"
    openDocument(server, uri, "\\u")
    val labels = requestCompletion(server, uri, 2)
    assert(labels.exists(_.contains("002C")), "should include comma")
    assert(labels.exists(_.contains("003A")), "should include colon")
    assert(labels.exists(_.contains("0028")), "should include left paren")
  }

  test("completion: \\u with hex prefix filters") {
    val server = newServer
    val uri = "expr:uc2"
    openDocument(server, uri, "\\u002")
    val labels = requestCompletion(server, uri, 5)
    assert(labels.exists(_.contains("002C")), "should include comma")
    assert(!labels.exists(_.contains("003A")), "003A doesn't start with 002")
  }

  test("completion: \\u with exact 4-digit hex") {
    val server = newServer
    val uri = "expr:uc3"
    openDocument(server, uri, "\\u0041")
    val labels = requestCompletion(server, uri, 6)
    // 0041 is 'A' — LATIN CAPITAL LETTER A
    assert(labels.exists(_.contains("0041")))
  }

  test("completion: \\u with name search") {
    val server = newServer
    val uri = "expr:uc4"
    openDocument(server, uri, "\\uarrow")
    val labels = requestCompletion(server, uri, 7)
    assert(labels.nonEmpty, "should find arrow characters")
    assert(labels.forall(_.toLowerCase.contains("arrow")))
  }

  test("completion: \\u in middle of expression") {
    val server = newServer
    val uri = "expr:uc5"
    openDocument(server, uri, "a,\\u")
    val labels = requestCompletion(server, uri, 4)
    assert(labels.exists(_.contains("002C")), "should show curated list after comma")
  }
}
