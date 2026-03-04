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
}