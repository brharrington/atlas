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
import java.util.concurrent.atomic.AtomicReference

import com.netflix.atlas.core.stacklang.Interpreter
import com.netflix.atlas.core.stacklang.Vocabulary
import org.eclipse.lsp4j.CompletionOptions
import org.eclipse.lsp4j.InitializeParams
import org.eclipse.lsp4j.InitializeResult
import org.eclipse.lsp4j.ServerCapabilities
import org.eclipse.lsp4j.services.LanguageClient
import org.eclipse.lsp4j.services.LanguageClientAware
import org.eclipse.lsp4j.services.LanguageServer
import org.eclipse.lsp4j.services.TextDocumentService
import org.eclipse.lsp4j.services.WorkspaceService

class AtlasLspServer(vocabulary: Vocabulary) extends LanguageServer with LanguageClientAware {

  private val clientRef = new AtomicReference[LanguageClient]

  def client: LanguageClient = clientRef.get()

  val interpreter: Interpreter = Interpreter(vocabulary.allWords)

  private val textDocService = new AtlasTextDocumentService(interpreter)
  private val workspaceService = new AtlasWorkspaceService

  override def initialize(params: InitializeParams): CompletableFuture[InitializeResult] = {
    val capabilities = new ServerCapabilities
    val completionOptions = new CompletionOptions
    completionOptions.setTriggerCharacters(java.util.List.of(",", ":"))
    capabilities.setCompletionProvider(completionOptions)
    val result = new InitializeResult(capabilities)
    CompletableFuture.completedFuture(result)
  }

  override def shutdown(): CompletableFuture[AnyRef] = {
    CompletableFuture.completedFuture(null)
  }

  override def exit(): Unit = {}

  override def getTextDocumentService: TextDocumentService = textDocService

  override def getWorkspaceService: WorkspaceService = workspaceService

  override def connect(client: LanguageClient): Unit = {
    clientRef.set(client)
  }
}