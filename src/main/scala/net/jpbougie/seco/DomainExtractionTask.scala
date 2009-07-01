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
abstract class DomainExtractionTask extends Task with Configurable {
  
  var applyTo = ""
  
  def setup(config: ConfigMap) = {
    this.applyTo = config.getString("apply_to", "")
  }
  
  
  def extract(parts: List[Part]): List[List[String]]
  
  def run(params: Map[String, ExecutionProfile]): JsValue = {
        
    val parts = (list ! obj)(params(this.applyTo).result.get).map(Part.fromJson(_))
    
    val result = extract(parts)
    
    JsArray(result.map(d => { JsArray(d.map(JsString(_))) }))
  }
  
}
