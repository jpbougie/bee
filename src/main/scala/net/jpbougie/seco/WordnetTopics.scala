package net.jpbougie.seco

import utils.Wordnet

import scala.collection.immutable.Map

import net.jpbougie.bee.{Task, ExecutionProfile, Configurable}

import dispatch._
import dispatch.json._
import Js._

import net.lag.configgy._

class WordnetTopics extends DomainExtractionTask {
  
  var wordnetDir = ""
  var domainsFile = ""
  var wordnet: Wordnet = null
  
  override def identifier = "wordnettopics-" + this.applyTo
  override def version = "1"
  
  override def setup(config: ConfigMap) = {
    super.setup(config)
    this.wordnetDir = config.getString("wordnet_dir", "")
    this.domainsFile = config.getString("domains_file", "")    
    
    this.wordnet = new Wordnet(wordnetDir, domainsFile)
  }
  
  def extract(parts: List[Part]): List[List[String]] = {
        
    parts.map { part => part.objects.flatMap { 
        case ("noun", value) => wordnet.nouns.getTopics(value).flatten
        case ("verb", value) => wordnet.verbs.getTopics(value).flatten
    } }
  }
}
