package net.jpbougie.seco

// The strategy of this splitting technique is to find all the S, SQ and take the first VP and NP in it.
// stop and start a new domain when a new S is found
class ClauseSplit extends SplittingTask {
  
  override def identifier = "clausesplit"
  override def version = "1"
  
  def split(tree: Tree): List[Part] = {
    val interesting = List("S", "SQ")
    tree match {
      case Leaf(label, value) => Nil
      case Node(label, children) if interesting.contains(label) => {
        val (before, after) = children.toList.break({
          case Node(label, children) => interesting.contains(label)
          case _ => false
        })
        
        var phrase = before.flatMap { subtree => subtree.leaves.map(_.value) }
        
        Part(phrase.mkString(" "), extractInteresting(before.toList)) :: after.toList.flatMap(split(_))
      }
      
      case Node(label, children) => children.toList flatMap { child => split(child) }
    }
  }
  
  def extractInteresting(children: List[Tree]): List[(String, String)] = {
    children flatMap {
      case Leaf(label, value) if label.startsWith("NN") => ("noun" -> value) :: Nil
      case Leaf(label, value) if label.startsWith("VB") => ("verb" -> value) :: Nil
      case Node(label, children) => extractInteresting(children.toList)
      case _ => Nil
    }
  }
}