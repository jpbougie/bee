package net.jpbougie.seco

import net.jpbougie.bee.{Task, ExecutionProfile}

import dispatch._
import dispatch.json._
import Js._


/* The strategy of this heuristic is to take each first-level separation as a different domain
 * and extract the nouns from it.
 */
abstract class SplittingTask extends Task {
  
  def split(tree: Tree): List[Part]
  
  def run(params: Map[String, ExecutionProfile]): JsValue = {
    
    val tree = Tree.fromJson(params("stanford").result.get)
    
    val result = split(tree)
    
    JsArray(result map {part => part.toJson})
  }
}
