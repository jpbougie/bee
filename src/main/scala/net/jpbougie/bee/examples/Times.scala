package net.jpbougie.bee.examples

import dispatch._
import dispatch.json._
import Js._

import net.lag.configgy._

class Times extends Task with Configurable {
  var times: Int = 1
  
  def setup(config: ConfigMap) = {
    times = config.getInt("times").getOrElse(1)
  }
  
  def run(params: Map[String, ExecutionProfile]): JsValue = {
    val numx = 'num ? num
    val numx(x) = params("input").result.get
    
    JsNumber(x * times)
  }
  
  override def identifier = "times"
}