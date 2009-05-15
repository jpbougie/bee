package net.jpbougie.bee.examples

import dispatch._
import dispatch.json._
import Js._

class Double extends Task {
  def run(params: JsValue): JsValue = {
    val numx = 'num ? num
    val numx(x) = params
    
    JsNumber(x * 2)
  }
  
  def identifier = "double"
}