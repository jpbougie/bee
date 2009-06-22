package net.jpbougie.seco

import net.jpbougie.bee.{Task, ExecutionProfile}

import dispatch._
import dispatch.json._
import Js._


/* The strategy of this heuristic is to take each first-level separation as a different domain
 * and extract the nouns from it.
 */
class ParseStats extends Task {
  
  override def identifier = "stats"
  override def version = "1"
  
  def run(params: Map[String, ExecutionProfile]): JsValue = {
    
    val tree = Tree.fromJson(params("stanford").result.get)
    
    val stats: List[(JsValue => JsObject)] = ('depth << tree.depth) ::
                                             ('number_of_nodes << tree.nodes.length) ::
                                             ('number_of_leaves << tree.leaves.length) ::
                                             ('avg_children << (0 /: tree.nodes)(_ + _.children.length) / tree.nodes.length.asInstanceOf[Double] ) ::
                                             ('max_children << (0 /: tree.nodes) { (m, n) => Math.max(m, n.children.length) } ) ::
                                             ('number_of_nouns << tree.leaves.filter(isNoun(_)).length) ::
                                             ('number_of_subjonctives << tree.nodes.filter(_.label.equals("IN"))) ::
                                             Nil
    
    (JsObject() /: stats) { (js: JsObject, stat:(JsValue => JsObject)) => stat(js) }
  }
  
  def isNoun(leaf: Leaf): Boolean = {
    List("NN", "NNS", "NNP", "NNPS").contains(leaf.label)
  }
}
