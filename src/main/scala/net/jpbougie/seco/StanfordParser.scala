package net.jpbougie.seco

import net.jpbougie.bee.{Task, ExecutionProfile, Configurable}

import dispatch._
import dispatch.json._
import Js._
import net.lag.configgy.ConfigMap
import edu.stanford.nlp.parser.lexparser.LexicalizedParser
import edu.stanford.nlp.trees.{Tree => StanTree}

class StanfordParser extends Task with Configurable {
  var parser: LexicalizedParser = null
  
  override def identifier = "stanford"
  override def version = "1"
  
  def setup(config: ConfigMap) = {
    parser = new LexicalizedParser(config.getString("datafile", "englishPCFG.ser.gz"))
  }
  
  def run(params: Map[String, ExecutionProfile]): JsValue = {
    val linex = 'question ? str
    val linex(question) = params("input").result.get
    
    val tree = parser.apply(question)
    
    System.gc
    toJson(tree)
  }
  
  private def toJson(tree: StanTree): JsObject = {
    val score = ('score << tree.score)
    val label = ('label << tree.label.toString)
    val children = ('children << (tree.children.map(toJson(_))))
    
    
    val res = score(label(Js()))
    
    if(tree.children.length > 0) {
      children(res)
    } else {
      res
    }
  }
}