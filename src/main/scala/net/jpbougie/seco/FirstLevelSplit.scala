package net.jpbougie.seco

import net.jpbougie.bee.{Task, ExecutionProfile}

import dispatch._
import dispatch.json._
import Js._


/* The strategy of this heuristic is to take each first-level separation as a different domain
 * and extract the nouns from it.
 */
class FirstLevelSplit extends SplittingTask {
  
  override def identifier = "firstlevelsplit"
  override def version = "2"
  
  def split(tree: Tree): List[Part] = {
    
    skipUniqueChild(tree, 3) match {
      case Node(label, children) => children.toList.map({ subtree => Part(subtree.leaves.map(_.value) mkString " ", findNouns(subtree).map("noun" -> _))})
      case Leaf(label, value) => Part(value, ("noun" -> value) :: Nil) :: Nil
    }
  }
  
  /**
   * This function will take a tree and extract the leaves that are nouns (labels NN, NNS, NNP, NNPS)
   */
  def findNouns(tree : Tree): List[String] = {
    tree match {
      case Leaf(label, value) if List("NN", "NNS", "NNP", "NNPS").contains(label) => value :: Nil
      case Leaf(_, _) => Nil
      case Node(_, children) => children.flatMap(findNouns(_)).toList
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
