package net.jpbougie.seco

import utils.Wordnet

import scala.collection.immutable.Map

import net.jpbougie.bee.{Task, ExecutionProfile, Configurable}

import dispatch._
import dispatch.json._
import Js._

import net.lag.configgy._


/* Uses the WordNet database to extract a synset and then WordNet Domains to fetch it most probably domain */

/*
 * The hierarchy is as follows :
    In input, this task takes a list of parts of the question, extracted from the previous task
    Each part is composed of one or more objects, noun which appear directly in the text
    From each object, we extract its senses from WordNet
    From each sense that we obtained, we extract one or more domain given by WordNet-Domains
 */
class WordnetDomains extends DomainExtractionTask {
  
  var wordnetDir = ""
  var domainsFile = ""
  var wordnet: Wordnet = null
  
  override def identifier = "wordnet-" + this.applyTo
  override def version = "1"
  
  override def setup(config: ConfigMap) = {
    super.setup(config)
    this.wordnetDir = config.getString("wordnet_dir", "")
    this.domainsFile = config.getString("domains_file", "")    
    
    this.wordnet = new Wordnet(wordnetDir, domainsFile)
  }
  
  def extract(parts: List[Part]): List[List[String]] = {
        
    var expandedParts = parts.map { part => part.objects.map { 
        case ("noun", value) => wordnet.nouns.getDomains(value)
        case ("verb", value) => wordnet.verbs.getDomains(value)
    } }
    
    val partsWithMaps = expandedParts.map { part => (part, tf(part), idf(part)) }
    
    for((p, tf, idf) <- partsWithMaps)
        yield tf.toList.map { case (dom, tfScore) => (dom, tfScore * idf(dom)) }.sort(_._2 > _._2).map(_._1)
    
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
  
  // calculates the total frequency
  def tf(part: List[List[List[String]]]): Map[String, Double] = {
    val numberOfDomains = part map { obj => obj.foldLeft(0)(_ + _.length) } // returns the total number of domains for each object
    
    part.zip(numberOfDomains).foldLeft(Map[String, Double]().withDefaultValue(0.0)) { (m: Map[String, Double], tuple: (List[List[String]], Int)) =>
        def foldDomains(map: Map[String, Double], dom: String, nbrDomains: Int): Map[String, Double] = 
                          { map + (dom -> (map(dom) + (1 / nbrDomains.asInstanceOf[Double]): Double)) }
        tuple._1.foldLeft(m) { (m: Map[String, Double], sense: List[String]) => (sense.foldLeft(m)(foldDomains(_, _, tuple._2))) }
      }
  }
  
}
