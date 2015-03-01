package org.creak.docversioning.storage

import org.creak.docversioning.JVerDocument

/**
 * Created by mgcuthbert on 3/1/2015.
 */
trait StorageFactory {
  this: JVerDocument =>

  implicit val storageClass:Class[_]

  def writeDocument() = {

  }

  def saveDocument() = {

  }
}
