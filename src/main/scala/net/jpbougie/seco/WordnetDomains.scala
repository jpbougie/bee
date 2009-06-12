package net.jpbougie.seco

import scala.io.Source
import scala.collection.immutable.IntMap

import net.jpbougie.bee.{Task, ExecutionProfile, Configurable}

import dispatch._
import dispatch.json._
import Js._

import net.lag.configgy._

import edu.smu.tspell.wordnet._
import edu.smu.tspell.wordnet.impl.file.synset._

/* Uses the WordNet database to extract a synset and then WordNet Domains to fetch it most probably domain */
class WordnetDomains extends Task with Configurable {
  
  var applyTo = ""
  var wordnetDir = ""
  var domainsFile = ""
  var domainMap: IntMap[Array[String]] = IntMap.empty
  
  override def identifier = "wordnet-" + this.applyTo
  
  def setup(config: ConfigMap) = {
    this.applyTo = config.getString("apply_to", "")
    this.wordnetDir = config.getString("wordnet_dir", "")
    this.domainsFile = config.getString("domains_file", "")
    
    System.setProperty("wordnet.database.dir", this.wordnetDir)
    
    val source = Source.fromFile(this.domainsFile)
    
    // A line is xxxxxxxx-t[TAB](ddddddd...)([SPACE]ddddd...)*
    // where xxxxxxxx is the offset, t is the type, dddd are domains
    // we only keep the 'n' type (for nouns)
    val it = source.getLines.filter(_(9) == 'n')
                            .map(l => (l.take(8).toInt, l.split("\t").last.split(" ")))
                            
    this.domainMap = IntMap.empty ++ new Iterable[(Int, Array[String])] { def elements = it } 
  }
  
  def run(params: Map[String, ExecutionProfile]): JsValue = {
    
    var db = WordNetDatabase.getFileInstance
    
    val objects = (list ! (list ! str))(params(this.applyTo).result.get)
    
    var domains = for(domain <- objects)
      /* take the last object in the domain, as it is most likely to be the dominant one */
      yield db.getSynsets(domain.last, SynsetType.NOUN).first match {
        case x:NounReferenceSynset => this.domainMap(x.getOffset).first
      }
    
    JsArray(domains.map(JsString(_)))
  }
}
