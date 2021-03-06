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

package org.creak.docversioning

import org.scalatest.FlatSpec
import net.liftweb.json._

/**
 * Created by mgcuthbert on 2/27/2015.
 */
class JVerSpec extends FlatSpec {

  val doc = JVerDoc("test1", 5, Map(
    "prop1" -> List(FieldChange("prop1", "val1", 1)),
    "prop2" -> List(
      FieldChange("prop2", "val2", 1),
      FieldChange("prop2", "update2", 2),
      FieldChange("prop2", "val56", 3),
      FieldChange("prop2", "78", 4),
      FieldChange("prop2", "finalValue", 5)
    ),
    "prop3" -> List(
      FieldChange("prop3", "val3", 1),
      FieldChange("prop3", "endValue", 5)
    ),
    "prop4" -> List(
      FieldChange("prop4", "p4", 1),
      FieldChange("prop4", "changed", 3)
    )
  ))

  case class Prop(intP:Int, strP:String, doubleP:Double)

  val doc2 = JVerDoc("test2", 1, Map(
    "StringProp" -> List(FieldChange("StringProp", "stringvalue", 1)),
    "IntProp" -> List(FieldChange("IntProp", 1, 1)),
    "DoubleProp" -> List(FieldChange("DoubleProp", 1.0, 1)),
    "CaseClassProp" -> List(FieldChange("CaseClassProp", Prop(1, "test", 1.0), 1))
  ))

  "A JVerDoc" should "update EVERY property correctly" in {
    val updates = Map(
      "StringProp" -> "upValue",
      "IntProp" -> 4,
      "DoubleProp" -> 4.5,
      "CaseClassProp" -> Prop(4, "test2", 2.0)
    )
    val upDoc = doc2.update(updates)
    val changes = upDoc.getChanges(List("StringProp", "IntProp", "DoubleProp", "CaseClassProp"))
    changes foreach {
      x =>
        assert(x.version == 2, "Version of Field change should be 2")
        if (x.propertyName.equals("StringProp")) {
          assert(x.value == "upValue")
        } else if (x.propertyName.equals("IntProp")) {
          assert(x.value == 4)
        } else if (x.propertyName.equals("DoubleProp")) {
          assert(x.value == 4.5)
        } else if (x.propertyName.equals("CaseClassProp")) {
          assert(x.value == Prop(4, "test2", 2.0))
        }
    }
  }

  it should "get the correct field change when requested" in {
    doc.getFieldChange("prop1", Some(1)) match {
      case Some(fc) => assert(fc.version == 1 && fc.value == "val1")
      case None => assert(1 == 2, "prop1 with version 1 could not be found")
    }

    doc.getFieldChange("prop2", Some(3)) match {
      case Some(fc) => assert(fc.version == 3 && fc.value == "val56")
      case None => assert(1 == 2, "prop2 with version 1 could not be found")
    }

    doc.getFieldChange("prop3") match {
      case Some(fc) => assert(fc.version == 5 && fc.value == "endValue")
      case None => assert(1 == 2, "prop2 with version 1 could not be found")
    }

    doc.getFieldChange("prop4") match {
      case Some(fc) => assert(fc.version == 3 && fc.value == "changed")
      case None => assert(1 == 2, "prop2 with version 1 could not be found")
    }
  }

  it should "get the property history correctly" in {
    doc.getPropertyHistory("prop1") foreach {
      x => if (x.version == 1) assert(x.value == "val1")
    }
    doc.getPropertyHistory("prop2") foreach {
      x =>
        if (x.version == 1) assert(x.value == "val2")
        else if (x.version == 2) assert(x.value == "update2")
        else if (x.version == 3) assert(x.value == "val56")
        else if (x.version == 4) assert(x.value == "78")
        else if (x.version == 5) assert(x.value == "finalValue")
        else assert(1==2, "unexpected version")
    }
  }

  it should "get the changes for a specific version" in {
    doc.getVersionChanges(3) foreach {
      x =>
        if (x._1 == "prop2") assert(x._2.version == 3 && x._2.value == "val56")
        else if (x._1 == "prop4") assert(x._2.version == 3 && x._2.value == "changed")
        else assert(1==2, "Unexpected value")
    }
  }

  it should "get a specific version of the document" in {
    doc.getChangeVersion(Some(3)) foreach {
      x =>
        if (x._1 == "prop1") assert(x._2.version == 1 && x._2.value == "val1")
        else if (x._1 == "prop2") assert(x._2.version == 3 && x._2.value == "val56")
        else if (x._1 == "prop3") assert(x._2.version == 1 && x._2.value == "val3")
        else if (x._1 == "prop4") assert(x._2.version == 3 && x._2.value == "changed")
        else assert(1==2, "Unexpected property")
    }
    doc.getChangeVersion() foreach {
      x =>
        if (x._1 == "prop1") assert(x._2.version == 1 && x._2.value == "val1")
        else if (x._1 == "prop2") assert(x._2.version == 5 && x._2.value == "finalValue")
        else if (x._1 == "prop3") assert(x._2.version == 5 && x._2.value == "endValue")
        else if (x._1 == "prop4") assert(x._2.version == 3 && x._2.value == "changed")
        else assert(1==2, "Unexpected property")
    }
  }

  it should "get a JSON version of the change versions" in {
    assert(compact(render(doc.getJSONVersion(Some(3)))) == """{"prop1":"val1","prop2":"val56","prop3":"val3","prop4":"changed"}""")
  }

  it should "get a case class version of the change versions" in {
    case class test(prop1:String, prop2:String, prop3:String, prop4:String)
    val t1 = doc.getVersion[test](Some(3))
    val t2 = test("val1", "val56", "val3", "changed")
    assert(t1 == t2)

    case class test2(StringProp:String, IntProp:Int, DoubleProp:Double, CaseClassProp:Prop)
    val te1 = doc2.getVersion[test2]()
    val te2 = test2("stringvalue", 1, 1.0, Prop(1, "test", 1.0))
    assert(te1.canEqual(te2))
  }
}
