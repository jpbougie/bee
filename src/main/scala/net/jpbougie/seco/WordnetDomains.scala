package net.jpbougie.seco

import scala.io.Source
import scala.collection.immutable.{IntMap, Map}

import net.jpbougie.bee.{Task, ExecutionProfile, Configurable}

import dispatch._
import dispatch.json._
import Js._

import net.lag.configgy._

import edu.smu.tspell.wordnet._
import edu.smu.tspell.wordnet.impl.file.Morphology
import edu.smu.tspell.wordnet.impl.file.synset._

/* Uses the WordNet database to extract a synset and then WordNet Domains to fetch it most probably domain */
class WordnetDomains extends Task with Configurable {
  
  var applyTo = ""
  var wordnetDir = ""
  var domainsFile = ""
  var domainMap: IntMap[Array[String]] = IntMap.empty
  var synsetsMap: Map[String, Seq[Int]] = Map.empty
  
  override def identifier = "wordnet-" + this.applyTo
  
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
    
    val objects = (list ! (list ! str))(params(this.applyTo).result.get)
    
    def getDomains(word: String): List[String] = {
      
      val words = ((word :: Nil) ++ Morphology.getInstance.getBaseFormCandidates(word, SynsetType.NOUN)
                                                         .filter(x => !(x equals word)))
                                    .map { _.toLowerCase.replaceAll(" ", "_") }
      
      for(w <- words if this.synsetsMap contains w;
          offset <- this.synsetsMap(w);
          domain <- this.domainMap(offset))
        yield domain
    }
    
    var domains = for(domain <- objects)
                    yield for(obj <- domain) 
                      yield getDomains(obj)
    
    JsArray(domains.map(d => { JsArray(d.map(JsString(_))) }))
  }
  
}
