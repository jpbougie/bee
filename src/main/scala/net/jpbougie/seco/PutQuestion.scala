package net.jpbougie.seco

import net.jpbougie.bee.Task

import dispatch._
import dispatch.json._
import Js._

class PutQuestion extends Task {
  
  def run(params: JsValue): JsValue = {
    val linex = 'question ? str
    val linex(question) = params
    
    JsString(question)
  }
}