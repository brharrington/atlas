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

import com.netflix.atlas.core.stacklang.StandardVocabulary
import org.eclipse.lsp4j.CompletionParams
import org.eclipse.lsp4j.DidOpenTextDocumentParams
import org.eclipse.lsp4j.InitializeParams
import org.eclipse.lsp4j.Position
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
    assert(labels.contains(":clear"))
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
    data.grouped(5).map { group =>
      pos += group(1) // deltaStart (deltaLine is always 0)
      (pos, group(2), group(3)) // (absoluteStart, length, tokenType)
    }.toList
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
    assertEquals(wordTokens(0), (2, 2, AtlasTokenTypes.Word))   // :d
    assertEquals(wordTokens(1), (9, 2, AtlasTokenTypes.Word))   // up
    assertEquals(commentTokens.head, (4, 5, AtlasTokenTypes.Comment)) // /*c*/
  }
}