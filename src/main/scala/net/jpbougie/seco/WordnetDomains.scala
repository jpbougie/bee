package net.jpbougie.seco

import scala.io.Source
import scala.collection.immutable.{IntMap, Map, Set}

import net.jpbougie.bee.{Task, ExecutionProfile, Configurable}

import dispatch._
import dispatch.json._
import Js._

import net.lag.configgy._

import edu.smu.tspell.wordnet._
import edu.smu.tspell.wordnet.impl.file.Morphology
import edu.smu.tspell.wordnet.impl.file.synset._

/* Uses the WordNet database to extract a synset and then WordNet Domains to fetch it most probably domain */

/*
 * The hierarchy is as follows :
    In input, this task takes a list of parts of the question, extracted from the previous task
    Each part is composed of one or more objects, noun which appear directly in the text
    From each object, we extract its senses from WordNet
    From each sense that we obtained, we extract one or more domain given by WordNet-Domains
 */
class WordnetDomains extends Task with Configurable {
  
  var applyTo = ""
  var wordnetDir = ""
  var domainsFile = ""
  var domainMap: IntMap[Array[String]] = IntMap.empty
  var synsetsMap: Map[String, Seq[Int]] = Map.empty
  
  override def identifier = "wordnet-" + this.applyTo
  override def version = "1"
  
  def setup(config: ConfigMap) = {
    this.applyTo = config.getString("apply_to", "")
    this.wordnetDir = config.getString("wordnet_dir", "")
    this.domainsFile = config.getString("domains_file", "")
    
    System.setProperty("wordnet.database.dir", this.wordnetDir)
    
    /* Parse the noun index file from WordNet
     * This is done instead of using JAWS because it bugs on some normal-looking entries 
     */
    
    val sourceIndex = Source.fromFile(this.wordnetDir + "/index.noun")
    
    val iterIndex = sourceIndex.getLines.dropWhile( l => l startsWith "  ") // 2-space lines are comments
                                        .map({ l => 
                                          val elems = l.stripLineEnd.split(" ").filter(!_.isEmpty )
                                          val noun = elems(0)
                                          val count = elems(2).toInt
                                          (noun, elems.toList.takeRight(count).map(_ toInt))
                                        })
    synsetsMap = Map.empty ++ new Iterable[(String, Seq[Int])] { def elements = iterIndex }
    
    
    /* Parse the synset - domain association */
    
    val sourceDomains = Source.fromFile(this.domainsFile)
    
    // A line is xxxxxxxx-t[TAB](ddddddd...)([SPACE]ddddd...)*
    // where xxxxxxxx is the offset, t is the type, dddd are domains
    // we only keep the 'n' type (for nouns)
    val iterDomains = sourceDomains.getLines.filter(_(9) == 'n')
                                   .map(l => (l.take(8).toInt, l.stripLineEnd.split("\t").last.split(" ")))
                            
    this.domainMap = IntMap.empty ++ new Iterable[(Int, Array[String])] { def elements = iterDomains } 
  }
  
  def run(params: Map[String, ExecutionProfile]): JsValue = {
    
    var db = WordNetDatabase.getFileInstance
    
    val parts = (list ! (list ! str))(params(this.applyTo).result.get)
    
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
    
    // calculates the inverse document frequency for each domain included in a part
    // for reference (part -> object -> sense -> domain)
    def idf(part: List[List[List[String]]]): Map[String, Double] = {
      /*
       * Formula for idf
       * idf(i) = log(D/a(i))
       * where i is a domain, D is the total number of senses for that part
       * a(i) is the number of senses in which the domain appears
       */
       
      val D = part.length.asInstanceOf[Double]
      
      def extractDomainsFromObject(o: List[List[String]]): Set[String] = { 
        o.foldLeft(Set.empty[String]) { (set:Set[String], sense: List[String]) => (set ++ sense) }
      }
      
      val map = part.foldLeft(Map[String, Int]().withDefaultValue(0)) { (m:Map[String, Int], obj:List[List[String]]) => m ++ extractDomainsFromObject(obj).toList.map { d => (d -> (m(d) + 1))} }
      
      map transform {(domain, appearances) => Math.log(D / appearances.asInstanceOf[Double]) }
    }
    
    def tf(part: List[List[List[String]]]): Map[String, Double] = {
      val numberOfDomains = part map { obj => obj.foldLeft(0)(_ + _.length) } // returns the total number of domains for each object
      
      part.zip(numberOfDomains).foldLeft(Map[String, Double]().withDefaultValue(0.0)) { (m: Map[String, Double], tuple: (List[List[String]], Int)) =>
          def foldDomains(map: Map[String, Double], dom: String, nbrDomains: Int): Map[String, Double] = 
                            { map + (dom -> (map(dom) + (1 / nbrDomains.asInstanceOf[Double]): Double)) }
          tuple._1.foldLeft(m) { (m: Map[String, Double], sense: List[String]) => (sense.foldLeft(m)(foldDomains(_, _, tuple._2))) }
        }
    }
    
    var expandedParts = parts.map { obj => obj.map(getDomains(_)) }
    
    val partsWithMaps = expandedParts.map { part => (part, tf(part), idf(part)) }
    
    val result = for((p, tf, idf) <- partsWithMaps)
                  yield tf.toList.map { case (dom, tfScore) => (dom, tfScore * idf(dom)) }.sort(_._2 > _._2).map(_._1)
    
    JsArray(result.map(d => { JsArray(d.map(JsString(_))) }))
  }
  
}
