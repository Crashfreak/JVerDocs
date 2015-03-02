/*
 * Copyright 2015 Michael Cuthbert
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

package org.creak.docversioning.storage

import org.creak.docversioning.JVerDoc

import scala.collection.mutable

/**
 * Adds functions that automatically manages the storage of the documents in memory
 *
 * @author Michael Cuthbert on 2/25/15.
 */
class MemoryDoc extends StorageMech {
  val memoryMap = mutable.Map[String, JVerDoc]()

  /**
   * Saves the document to whichever storage for the class
   *
   * @param doc
   * @return
   */
  override def saveDocument(doc: JVerDoc): Unit = memoryMap.put(doc._id, doc)

  /**
   * Loads the document to whichever storage for the class
   *
   * @param id
   * @return
   */
  override def loadDocument(id: String): Option[JVerDoc] = memoryMap.get(id)
}
