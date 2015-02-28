/*
 * Copyright 2015 Michael Cuthbert
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */
package org.creak.docversioning

import java.util.UUID

import net.liftweb.json.JsonAST.JValue
import net.liftweb.json._
import scala.collection._

/**
 * A JVerDocument basically allows a user to maintain a version history of the document from storage.
 * The concept is quite simple, each property on the class gets versioned when an update is performed
 * on the doc. You can then retrieve specific versions on request. The document doesn't care where
 * it is stored and even supplies helper methods to convert to various storage formats
 *
 * It uses lift json to manage the document
 *
 * Format of the document is as follows:
 * {
 *   "_id":"<UUID>",
 *   "version":5,
 *   "property1": {
 *     "1":"key1",
 *     "4":"key2"
 *   },
 *   "property2": {
 *     "1":"key1"
 *   },
 *   "property3": {
 *     "1":"key1",
 *     "5":"key2"
 *   }
 * }
 *
 * The idea is to hide the versioning process from the user entirely, and a user simply has to request the
 * document and optionally what version they wish to few, or how many versions are available. The user can then
 * also request the diff of specific versions to see what has changed from document to document.
 *
 * @param json the json value to start from
 *             the default json builds the _id field and the versionLimit field for you
 *
 * @author Michael Cuthbert on 2/23/15.
 */
class JVerDocument[T](json:JValue/*=
  s"""
    |{
    | "_id":"${UUID.randomUUID().toString}",
    | "versionLimit":0
    |}""".stripMargin*/) {

  implicit val formats = DefaultFormats

  // stores all the documents in here
  // it lazy loads versions as the versions are requested
  val versionHistory = mutable.Map[Int, List[FieldChange]]()

  // version 0 is the unsaved version of the document
  var version = 0
  var id = UUID.randomUUID().toString

  /**
   * Gets the latest document available
   *
   * @return
   */
  //def getLatestDocument(includeID:Boolean=false) : T = getDocument(version, includeID)

  /**
   * Gets the document for a specified version
   *
   * @param v The version of the document you are looking for
   * @return
   */
  /*def getDocument(v:Int, includeID:Boolean=false) : T = {
    if (versionHistory.contains(v)) {
      //versionHistory.get(v).get
      {}.asInstanceOf[T]
    } else {
      val retJSON =
        s"""
          ${
          json map {
            x => x
          }
        }
        """.stripMargin
      val newObj = parse(retJSON).extract[T]
      //versionHistory.put(v, newObj)
      newObj
    }
  }

  def updateDocument(newData:T) = {
    val upVersion = version + 1
    val upJSON =
      s"""
        "${JVerDocument.KeyVersion}":"$upVersion",
        "${JVerDocument.KeyID}":"$id",
        ${
        json map {
          x => x
        }
      }
      """.stripMargin
    parse(upJSON).extract[T]
  }*/
}

object JVerDocument {
  val KeyVersion = "version"
  val KeyID = "_id"
}
