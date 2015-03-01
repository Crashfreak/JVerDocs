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

/**
 * @author Michael Cuthbert on 2/25/15.
 */
case class JVerDoc(_id:String, version:Int, changes:Map[String, List[FieldChange]], owner:Option[String]=None) {

  /**
   * Gets the field changes for a specific version
   *
   * @param props The list of properties that you are looking for
   * @param version The version you want, if None then will use the latest version
   * @return
   */
  def getChanges(props:List[String], version:Option[Int]=None) : List[FieldChange] = props flatMap { x => getFieldChange(x, version) }

  /**
   * Gets the field change for a specific version
   *
   * @param prop Property you are requesting
   * @param version the version you want, if None then will use the latest version
   * @return
   */
  def getFieldChange(prop:String, version:Option[Int]=None) : Option[FieldChange] = {
    changes.get(prop) match {
      case Some(v) =>
        val reqVer = version match {
          case Some(ver) => ver
          case None => this.version
        }
        Some(v flatMap { x => if (x.version <= reqVer) Some(x) else None } sortBy(- _.version) head)
      case None => None
    }
  }

  /**
   * Updates the document and returns a new JVerDoc
   *
   * @param newUpdates
   * @return
   */
  def update(newUpdates:Map[String, Any], userId:Option[String]=None) : JVerDoc = {
    val newVersion = version+1
    val upChanges = changes map {
      x =>
        val nList = newUpdates.get(x._1) match {
          case Some(nu) => List(FieldChange(x._1, nu, newVersion, userId))
          case None => List.empty[FieldChange]
        }
        val newList = x._2 ::: nList
        x._1 -> newList
    }
    this.copy(_id, newVersion, upChanges)
  }

  /**
   * Gets the property history for a specific property
   *
   * @param prop The property you are searching for
   * @param history how far back you want to go, 0 means infinite
   * @return A list of changes for the property
   */
  def getPropertyHistory(prop:String, history:Int=0) : List[FieldChange] = {
    changes.get(prop) match {
      case Some(l) =>
        history match {
          case 0 => l
          case _ => l.reverse.slice(0, history)
        }
      case None => List.empty[FieldChange]
    }
  }

  /**
   * Gets all the changes for a specific version
   *
   * @param version
   * @return
   */
  def getVersionChanges(version:Int) : Map[String, FieldChange] = {
    changes flatMap {
      case x =>
        val topVersion = x._2 flatMap { y => if (y.version == version) Some(y) else None }
        topVersion.length match {
          case 0 => None
          case _ => Some(x._1 -> topVersion.head)
        }
    }
  }

  /**
   * Quick helper function to get the latest version, just uses "getVersion" with the current version
   *
   * @return
   */
  def getLatestVersion : Map[String, FieldChange] = getVersion(version)

  /**
   * Gets the version and it's changes. This differs from getVersionChanges because it will get the latest
   * changes for every single version and not just the changes for the version you are looking for
   *
   * @param version
   * @return
   */
  def getVersion(version:Int) : Map[String, FieldChange] = {
    changes map {
      case x =>
        val topVersion = x._2 flatMap { y => if (y.version <= version) Some(y) else None} sortBy(- _.version)
        x._1 -> topVersion.head
    }
  }
}
case class FieldChange(propertyName:String, value:Any, version:Int, user:Option[String]=None)
