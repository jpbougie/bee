package net.jpbougie.seco

// The strategy of this splitting technique is to find all the S, SQ and take the first VP and NP in it.
// stop and start a new domain when a new S is found
class ClauseSplit extends SplittingTask {
  
  override def identifier = "clausesplit"
  override def version = "4"
  
  val interesting = List("S", "SQ", "FRAG")
  
  private def recurse(elems: List[Tree]): List[Part] = {
    
    elems match {
      case Nil => Nil
      case x => {
        val (before, after) = elems.break {
          case Node(label, _) => interesting.contains(label)
          case _ => false
        }

        val phrase = before.filter(_.isInstanceOf[Leaf]).map { case Leaf(_, value) => value }.mkString(" ")
        
        Part(phrase, extractInteresting(before.toList)) :: recurse(after match { case (x :: rest) => rest; case _ => after})
      }
    }
  }
  
  def split(tree: Tree): List[Part] = {
    recurse(tree.all.dropWhile { case Node(label, _) => !interesting.contains(label); case Leaf(_,_) => true } match {
      case (x :: rest) => rest
      case Nil => tree.all
    })
  }
  
  def extractInteresting(children: List[Tree]): List[(String, String)] = {
    children.filter(_.isInstanceOf[Leaf]) flatMap {
      case Leaf(label, value) if label.startsWith("NN") => ("noun" -> value) :: Nil
      case Leaf(label, value) if label.startsWith("VB") => ("verb" -> value) :: Nil
      case _ => Nil
    }
  }
}