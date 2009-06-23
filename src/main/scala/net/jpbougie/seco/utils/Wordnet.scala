package net.jpbougie.seco.utils

import scala.io.Source
import scala.collection.immutable.{IntMap, Map, Set}

import edu.smu.tspell.wordnet._
import edu.smu.tspell.wordnet.impl.file.Morphology
import edu.smu.tspell.wordnet.impl.file.synset._

class Wordnet(wordnetDir: String, domainsFile: String) {
  val domainMap: IntMap[Array[String]] = IntMap.empty
  val synsetsMap: Map[String, List[Int]] = parseIndex("noun")
  
  System.setProperty("wordnet.database.dir", wordnetDir)
  
  
  // Returns the list of domains for each sense of a word
  def getDomains(word: String): List[List[String]] = {
    
    // get possible variations on a word, as a plural or a singular, and then sanitize them
    val words = ((word :: Nil) ++ Morphology.getInstance.getBaseFormCandidates(word, SynsetType.NOUN)
                                                       .filter(x => !(x equals word)))
                                  .map { _.toLowerCase.replaceAll(" ", "_") }
    
    for(w <- words if this.synsetsMap contains w; // for each variation that actually exists in the dictionary
        offset <- this.synsetsMap(w)) // get the senses of the word
        yield (for(domain <- this.domainMap(offset))
          yield domain).toList
  }
  
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
  
  def parseDomains: IntMap[Array[String]] = {
    /* Parse the synset - domain association */
    
    val sourceDomains = Source.fromFile(this.domainsFile)
    
    // A line is xxxxxxxx-t[TAB](ddddddd...)([SPACE]ddddd...)*
    // where xxxxxxxx is the offset, t is the type, dddd are domains
    // we only keep the 'n' type (for nouns)
    val iterDomains = sourceDomains.getLines.filter(_(9) == 'n')
                                   .map(l => (l.take(8).toInt, l.stripLineEnd.split("\t").last.split(" ")))
                            
    IntMap.empty ++ new Iterable[(Int, Array[String])] { def elements = iterDomains }
  }
}