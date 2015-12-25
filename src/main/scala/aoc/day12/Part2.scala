package aoc.day12

import io.IO

object Part2 extends App {
  /*
--- Part Two ---

Uh oh - the Accounting-Elves have realized that they double-counted everything red.

Ignore any object (and all of its children) which has any property with the value 
"red". Do this only for objects ({...}), not arrays ([...]).

    [1,2,3] still has a sum of 6.
    [1,{"c":"red","b":2},3] now has a sum of 4, because the middle object is ignored.
    {"d":"red","e":[1,2,3,4],"f":5} now has a sum of 0, because the entire structure is ignored.
    [1,"red",5] has a sum of 6, because "red" in an array has no effect.
*/
  import spray.json._
  import DefaultJsonProtocol._ 

  def sum(jsonContent: JsValue): Int = {
    // Helper functions to detect the "red" values
    def containsRedJsValue(jsonValue: JsValue): Boolean =
      jsonValue match {
        case JsObject(fields) =>
          false
        //containsRed(fields)
        case JsArray(elements) =>
          false
        case JsString(value)           => value == "red"
        case JsNumber(n)               => false
        case JsTrue | JsFalse | JsNull => false
      }
    def containsRed(fields: Map[String, JsValue]): Boolean =
      fields.keySet.contains("red") ||
        fields.values.exists { v => containsRedJsValue(v) }

    // Computation of the sum
    jsonContent match {
      case JsObject(fields) =>
        if (containsRed(fields)) {
          0
        } else {
          fields.values.map(sum(_)).sum
        }
      case JsArray(elements) =>
        elements.map(sum(_)).sum
      case JsString(value)           => 0
      case JsNumber(n)               => n.toInt
      case JsTrue | JsFalse | JsNull => 0
    }
  }

  val input = IO.getLines().mkString
  val input_json = input.parseJson
  println(s"New sum avoiding doubled red-entries: ${sum(input_json)}")
}