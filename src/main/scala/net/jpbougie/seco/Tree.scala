package net.jpbougie.seco

import dispatch._
import dispatch.json._
import Js._

abstract case class Tree() {
  def depth: Int = {
    this match {
      case Node(label, children) => 1 + (0 /: children) { (m, child) => Math.max(m, child.depth) }
      case Leaf(_, _) => 0
    }
  }
  
  def leaves: List[Leaf] = {
    this match {
      case Node(label, children) => children.toList.flatMap (_.leaves)
      case l@Leaf(label, value) => l :: Nil
    }
  }
  
  def nodes: List[Node] = {
    this match {
      case n@Node(label, children) => n :: children.toList.flatMap(_.nodes)
      case Leaf(label, value) => Nil
    }
  }
}
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