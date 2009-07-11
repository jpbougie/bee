package net.jpbougie.seco

import dispatch._
import dispatch.json._
import Js._
case class Part(phrase: String, objects: List[(String, String)]) {
  def toJson: JsValue = {
    ('part << phrase)(('objects << (objects.map({ obj => ('type << obj._1)(('value << obj._2)(Js()))})))(Js()))
  }
  
}

object Part {
  def fromJson(js: JsValue): Part = {
    val part = ('part ! str)
    val objects = ('objects ! (list ! obj))
    
    
    
    Part(part(js), objects(js).map { obj => (('type ! str)(obj), ('value ! str)(obj))})
  }
}

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
  
  def all: List[Tree] = {
    this :: (this match {
      case Leaf(_, _) => Nil
      case Node(_, children) => children.toList.flatMap(_.all)
    })
  }
}
case class Node(label: String, children: Seq[Tree]) extends Tree
case class Leaf(label: String, value: String) extends Tree

case object Tree {
  def fromJson(js: JsValue): Tree = {
    val children = ('children ? list)
    val label = ('label ! str)
    val score = ('score ! num)
    
    /* Trees always end with a node whose unique child has a 0 score */
    
    
    js match {
      case children(ch) if (ch.length == 1 && score(ch.first) == 0) => Leaf(label(js), label(ch.first))
      case children(ch) => Node(label(js), ch.map(fromJson(_)))
    }
  }
}