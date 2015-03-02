/*
 * Copyright 2015 Michael Cuthbert
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package org.creak.docversioning.storage

import org.creak.docversioning.{JVerDoc, JVerDocument}

/**
 * Created by mgcuthbert on 3/1/2015.
 */
trait StorageFactory[T] {
  this: JVerDocument[T] =>

  implicit val storageClass:Class[_]
  var storageMech:Option[StorageMech] = None

  private def getStorageMech() : StorageMech = {
    storageMech match {
      case Some(sm) => sm
      case None =>
        if (classOf[StorageMech].isAssignableFrom(storageClass)) {
          val sc = storageClass.newInstance().asInstanceOf[StorageMech]
          storageMech = Some(sc)
          sc
        } else {
          throw new Exception("Factory storage class not assignable from StorageMech")
        }
    }
  }

  /**
   * Loads the document via the storage class
   *
   * @param id
   * @return
   */
  def loadDocument(id:String) : Option[JVerDoc] = getStorageMech().loadDocument(id)

  /**
   * Saves the document via the storage class
   *
   * @return
   */
  def saveDocument() = getStorageMech().saveDocument(this.doc)
}
