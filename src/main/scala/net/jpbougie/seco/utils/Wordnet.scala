package net.jpbougie.seco.utils

import scala.io.Source
import scala.collection.immutable.{IntMap, Map, Set}
import scala.util.matching.Regex

class WordnetParser(wordnetDir: String, domainsFile: String) {
  def parseIndex(pos: String): Map[String, List[Int]] = {
    /* Parse the noun index file from WordNet
     * This is done instead of using JAWS because it bugs on some normal-looking entries 
     */
    
    val sourceIndex = Source.fromFile(this.wordnetDir + "/index." + pos)
    
    val iterIndex = sourceIndex.getLines.dropWhile( l => l startsWith "  ") // 2-space lines are comments
                                        .map({ l => 
                                          val elems = l.stripLineEnd.split(" ").filter(!_.isEmpty )
                                          val noun = elems(0)
                                          val count = elems(2).toInt
                                          (noun, elems.toList.takeRight(count).map(_ toInt))
                                        })
    Map.empty ++ new Iterable[(String, List[Int])] { def elements = iterIndex }
  }
  
  def parseDomains(pos: Char): IntMap[Array[String]] = {
    /* Parse the synset - domain association */
    
    val sourceDomains = Source.fromFile(this.domainsFile)
    
    // A line is xxxxxxxx-t[TAB](ddddddd...)([SPACE]ddddd...)*
    // where xxxxxxxx is the offset, t is the type, dddd are domains
    val iterDomains = sourceDomains.getLines.filter(_(9) == pos)
                                   .map(l => (l.take(8).toInt, l.stripLineEnd.split("\t").last.split(" ")))
                            
    IntMap.empty ++ new Iterable[(Int, Array[String])] { def elements = iterDomains }
  }
  
  def parseExceptions(pos: String): Map[String, String] = {
    val sourceIndex = Source.fromFile(this.wordnetDir + "/" + pos + ".exc")
    
    val iterIndex = sourceIndex.getLines.dropWhile( l => l startsWith "  ") // 2-space lines are comments
                                        .map({ l => 
                                          val elems = l.stripLineEnd.split(" ").filter(!_.isEmpty)
                                          (elems(0) -> elems(1))
                                        })
    Map.empty ++ new Iterable[(String, String)] { def elements = iterIndex }
  }
}

abstract case class PartOfSpeech() {
  
  def synsets: Map[String, List[Int]]
  def domains: IntMap[Array[String]]
  def exceptions: Map[String, String]
  def detachmentRules: Map[Regex, String]
  
  // Returns the list of domains for each sense of a word
  def getDomains(word: String): List[List[String]] = {
    
    // get possible variations on a word, as a plural or a singular, and then sanitize them
    val words = ((word :: Nil) ++ this.getBaseForms(word)
                                      .filter(x => !(x equals word)))
                                      .map { _.toLowerCase.replaceAll(" ", "_") }
    
    for(w <- words if this.synsets contains w; // for each variation that actually exists in the dictionary
        offset <- this.synsets(w)) // get the senses of the word
        yield (for(domain <- this.domains(offset))
          yield domain).toList
  }
  
  def getBaseForms(word: String): List[String] = {
    exceptions.get(word).toList ++ 
    detachmentRules.filter({case (pattern, _) => pattern.findFirstIn(word).isDefined })
                   .map({ case (pattern, value) => pattern.replaceFirstIn(word, value) })
  }
}

case class Noun(parser: WordnetParser) extends PartOfSpeech {
  
  private val _synsets = parser.parseIndex("noun")
  override def synsets = _synsets
  private val _domains = parser.parseDomains('n')
  override def domains = _domains
  private val _excs = parser.parseExceptions("noun")
  override def exceptions = _excs
  
  override def detachmentRules = Map( new Regex("ses$")  -> "s"
                                    , new Regex("xes$")  -> "x"
                                    , new Regex("zes$")  -> "z"
                                    , new Regex("ches$") -> "ch"
                                    , new Regex("shes$") -> "sh"
                                    , new Regex("men$")  -> "man"
                                    , new Regex("ies$")  -> "y"
                                    , new Regex("s$")    -> ""
                                    )
}

case class Verb(parser: WordnetParser) extends PartOfSpeech {
  private val _synsets = parser.parseIndex("verb")
  override def synsets = _synsets
  private val _domains = parser.parseDomains('v')
  override def domains = _domains
  private val _excs = parser.parseExceptions("verb")
  override def exceptions = _excs
  
  override def detachmentRules = Map( new Regex("ies$") -> "y"
                                    , new Regex("es$")  -> "e"
                                    , new Regex("es$")  -> ""
                                    , new Regex("ed$")  -> "e"
                                    , new Regex("ed$")  -> ""
                                    , new Regex("ing$") -> "e"
                                    , new Regex("ing$") -> ""
                                    , new Regex("s$")   -> ""
                                    )
}

class Wordnet(wordnetDir: String, domainsFile: String) {
  val parser = new WordnetParser(wordnetDir, domainsFile)

  val nouns = Noun(parser)
  val verbs = Verb(parser)
}