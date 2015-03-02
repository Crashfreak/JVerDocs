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

package org.creak.docversioning

import org.scalatest.FlatSpec

/**
 * @author Michael Cuthbert on 2/25/15.
 */
class MemoryDocSpec extends FlatSpec {

  case class SubCC(i:Int, s:String)
  case class Tester(i:Int, s:String, cc:SubCC)

  val data = Tester(1, "tester", SubCC(2, "tester2"))

  val mainDoc = DocumentFactory.newDocument(data)

  "JVerDocuments" should "returned the latest document" in {
    val testData = mainDoc.getLatestDocument()
    assert(testData.canEqual(data))
  }

  it should "be able to save and retrieved document from memory" in {
    mainDoc.saveDocument()
    val upJVer = new JVerDocument[Tester](mainDoc.getID)
    assert(mainDoc.getLatestDocument().canEqual(upJVer.getLatestDocument()))
  }

  it should "be able to update, save and load the document from memory" in {
    val upData = data.copy(s="test2")
    mainDoc.updateDocument(upData).saveDocument()

  }
}
