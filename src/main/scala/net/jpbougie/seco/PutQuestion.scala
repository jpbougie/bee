package net.jpbougie.seco

import net.jpbougie.bee.{Task, ExecutionProfile}

import dispatch._
import dispatch.json._
import Js._

class PutQuestion extends Task {
  
  def run(params: Map[String, ExecutionProfile]): JsValue = {
    val linex = 'question ? str
    val linex(question) = params("input").result.get
    
    JsString(question)
  }
}