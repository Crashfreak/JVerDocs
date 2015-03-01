package org.creak.docversioning.storage

import org.creak.docversioning.JVerDocument

/**
 * Created by mgcuthbert on 3/1/2015.
 */
abstract class StorageMech {

  implicit val doc:JVerDocument

  def writeDocument(doc:JVerDocument)
}
