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

import java.util.UUID

import org.creak.docversioning.storage.{MemoryDoc, StorageMech}

/**
 * @author Michael Cuthbert on 3/2/15.
 */
object DocumentFactory {

  var storageClass:Class[_<:StorageMech] = classOf[MemoryDoc]

  def apply(clazz:Class[_<:StorageMech]) = storageClass = clazz

  def newDocument[T](data:T, id:String=UUID.randomUUID().toString, owner:Option[String]=None)(implicit mf:Manifest[T]) : JVerDocument[T] = {
    // get the list of field changes
    val fieldChanges = getDocumentMap(data) flatMap {
      d =>
        if (d._1.equalsIgnoreCase("$outer")) None
        else Some(d._1 -> List(FieldChange(d._1, d._2, 1, owner)))
    }

    val doc = new JVerDoc(id, 1, fieldChanges, owner)
    new JVerDocument[T](id, storageClass, Some(doc))
  }

  def getDocumentMap[T](data:T) : Map[String, Any] = {
    (Map[String, Any]() /: data.getClass.getDeclaredFields) {(a, f) =>
      f.setAccessible(true)
      a + (f.getName -> f.get(data))
    }
  }
}
