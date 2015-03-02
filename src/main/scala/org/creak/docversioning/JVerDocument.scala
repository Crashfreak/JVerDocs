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

import org.creak.docversioning.storage.{StorageMech, MemoryDoc, StorageFactory}
import scala.collection._
import scala.reflect.ClassTag

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
 *   "owner":"<OWNER_ID>"
 *   "property1": [
 *      {"version":1,"value":"key1","userId":"<USER_ID>"},
 *      {"version":4,"value":"key2","userId":"<USER_ID>"}
 *   ],
 *   "property2": {
 *     {"version":1,"value":"key1","userId":"<USER_ID>"}
 *   },
 *   "property3": {
 *     {"version":1,"value":"key1","userId":"<USER_ID>"},
 *     {"version":5,"value":"key2","userId":"<USER_ID>"}
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
    implicit val storageClass:Class[_<:StorageMech]=classOf[MemoryDoc],
    private val _doc:Option[JVerDoc]=None
)(implicit mf:Manifest[T]) extends StorageFactory[T] {
  // stores all the documents in here
  // it lazy loads versions as the versions are requested
  val versionHistory = mutable.Map[Int, T]()

  val doc = _doc match {
    case Some(d) => d
    case None => loadDocument(id) match {
      case Some(dc) => dc
      case None => throw new Exception(s"Doc with id [$id] not found.")
    }
  }

  def getID = id

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
      val currentDoc = doc.getVersion(Some(v))
      versionHistory.put(v, currentDoc)
      currentDoc
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
    val current = doc.getChangeVersion()
    val dataMap = (Map[String, Any]() /: newData.getClass.getDeclaredFields) {(a, f) =>
      f.setAccessible(true)
      a + (f.getName -> f.get(newData))
    }
    val changes = dataMap flatMap {
      field =>
        if (field._1.equalsIgnoreCase("$outer")) None
        else {
          current.get(field._1) match {
            case Some(v) => if (v.canEqual(field._2)) None else Some(field._1 -> field._2)
            case None => Some(field._1 -> field._2)
          }
        }
    } toMap

    new JVerDocument[T](id, storageClass, Some(doc.update(changes, userId)))
  }
}
