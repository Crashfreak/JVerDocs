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
 *   "property1": [
 *      {"version":1,"value":"key1"},
 *      {"version":4,"value":"key2"}
 *   ],
 *   "property2": {
 *     {"version":1,"value":"key1"}
 *   },
 *   "property3": {
 *     {"version":1,"value":"key1"},
 *     {"version":5,"value":"key2"}
 *   }
 * }
 *
 * The idea is to hide the versioning process from the user entirely, and a user simply has to request the
 * document and optionally what version they wish to few, or how many versions are available. The user can then
 * also request the diff of specific versions to see what has changed from document to document.
 *
 * @author Michael Cuthbert on 2/23/15.
 */
class JVerDocument[T](
    id:String,
    private val _doc:Option[JVerDoc]=None
)(implicit mf:Manifest[T]) {

  implicit val formats = DefaultFormats

  // stores all the documents in here
  // it lazy loads versions as the versions are requested
  val versionHistory = mutable.Map[Int, T]()

  val doc = _doc match {
    case Some(d) => d
    case None => loadDocument(id)
  }

  /**
   * Loads the document from the storage mechanism that is supplied through the factory
   *
   * @param id
   * @return
   */
  def loadDocument(id:String) : JVerDoc = {
    JVerDoc("", 1, Map.empty[String, List[FieldChange]])
  }

  /**
   * Saves the document to the storage mechanism that is supplied through the factory
   */
  def saveDocument() = {

  }

  /**
   * Gets the latest document available
   *
   * @return
   */
  def getLatestDocument() : T = getDocument(doc.version)

  /**
   * Gets the document for a specified version
   *
   * @param v The version of the document you are looking for
   * @return
   */
  def getDocument(v:Int) : T = {

    if (versionHistory.contains(v)) {
      versionHistory.get(v).get
    } else {
      val changes = doc.getVersion(v)
      classOf[T].newInstance()
    }
  }

  /**
   * This is like a github commit, commits a set of updates to a JVerDocument. Only when
   * saveDocument is called will it actually save it to Storage
   *
   * @param newData
   * @param userId
   * @return
   */
  def updateDocument(newData:T, userId:Option[String]=None) : JVerDocument[T] = {
    val current = doc.getLatestVersion
    val changes = classOf[T].getDeclaredFields flatMap {
      field =>
        val fieldName = field.getName
        val upData = field.get(newData)
        current.get(field.getName) match {
          case Some(v) => if (v.canEqual(upData)) None else Some(fieldName -> upData)
          case None => Some(fieldName -> upData)
        }
    } toMap
    JVerDocument[T](doc.update(changes, userId))
  }
}

object JVerDocument {
  val KeyVersion = "version"
  val KeyID = "_id"

  def apply[T]()(implicit mf:Manifest[T]) = new JVerDocument[T](UUID.randomUUID().toString)
  def apply[T](id:String)(implicit mf:Manifest[T]) = new JVerDocument[T](id)
  def apply[T](jVer:JVerDoc)(implicit mf:Manifest[T]) = new JVerDocument[T](jVer._id, Some(jVer))
}
