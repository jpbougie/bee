package net.jpbougie.seco

import net.jpbougie.bee.Task

import dispatch._
import dispatch.json._
import Js._
import edu.stanford.nlp.parser.lexparser.LexicalizedParser
import edu.stanford.nlp.trees.Tree

class StanfordParser extends Task {
  val parser = new LexicalizedParser("englishPCFG.ser.gz")
  
  def run(params: JsValue): JsValue = {
    val linex = 'question ? str
    val linex(question) = params
    
    val tree = parser.apply(question)
    
    toJson(tree)
  }
  
  private def toJson(tree: Tree): JsObject = {
    val score = ('score << tree.score)
    val label = ('label << tree.label.toString)
    val children = ('children << (tree.children.map(toJson(_))))
    
    score(label(children(Js())))
  }
  
  def identifier = "stanford"
}