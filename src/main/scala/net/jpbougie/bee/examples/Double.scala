package net.jpbougie.bee.examples

import dispatch._
import dispatch.json._
import Js._

class Double extends Task {
  def run(params: Map[String, ExecutionProfile]): JsValue = {
    val numx = 'num ? num
    val numx(x) = params("input").result.get
    
    JsNumber(x * 2)
  }
  
  override def identifier = "double"
}