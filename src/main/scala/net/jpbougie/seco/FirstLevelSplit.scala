package net.jpbougie.seco

import net.jpbougie.bee.{Task, ExecutionProfile}

import dispatch._
import dispatch.json._
import Js._


/* The strategy of this heuristic is to take each first-level separation as a different domain
 * and extract the nouns from it.
 */
class FirstLevelSplit extends Task {
  
  /* move the JSON tree to a more compact representation */
  abstract case class Tree()
  case class Node(label: String, children: Seq[Tree]) extends Tree
  case class Leaf(label: String, value: String) extends Tree
  
  case object Tree {
    def fromJson(js: JsValue): Tree = {
      val children = ('children ! list)
      val label = ('label ! str)
      val score = ('score ! num)
      
      /* Trees always end with a node whose unique child has a 0 score */
      val ch = children(js)
      if(ch.length == 1 && score(ch.first) == 0) {
        Leaf(label(js), label(ch.first))
      } else {
        Node(label(js), ch.map(fromJson(_)))
      }
    }
  }
  
  override def identifier = "firstlevelsplit"
  
  def run(params: Map[String, ExecutionProfile]): JsValue = {
    
    val tree = skipUniqueChild(Tree.fromJson(params("stanford").result.get), 2)
    
    val objects = tree match {
      case Node(label, children) => children.map(findNouns(_))
      case Leaf(label, value) => (label :: Nil) :: Nil
    } 
       
    JsArray(objects.map(l => { JsArray(l.map(JsString(_)).toList)}).toList)
  }
  
  /**
   * This function will take a tree and extract the leaves that are nouns (labels NN, NNS, NNP, NNPS)
   */
  def findNouns(tree : Tree): Seq[String] = {
    tree match {
      case Leaf(label, value) if List("NN", "NNS", "NNP", "NNPS").contains(label) => value :: Nil
      case Leaf(_, _) => Nil
      case Node(_, children) => children.flatMap(findNouns(_))
    }
  }
  
  /**
   * The tree might contain one or more first level elements with only one child
   * @param tree - the tree
   * @param max - the max depth to go to
   */
  def skipUniqueChild(tree : Tree, max: Int): Tree = {
    if (max <= 0) return tree
    tree match {
      case Node(label, children) if children.length == 1 => skipUniqueChild(children.first, max - 1)
      case _ => tree
    }
  }
}
