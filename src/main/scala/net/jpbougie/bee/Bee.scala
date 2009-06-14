/*
 * Copyright (c) 2009, Jean-Philippe Bougie
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of the <organization> nor the
 *       names of its contributors may be used to endorse or promote products
 *       derived from this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY <copyright holder> ''AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL <copyright holder> BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package net.jpbougie.bee

import scala.actors.{Actor, Exit}
import scala.actors.Actor._
import scala.util.parsing.json.JSON

import scala.collection.immutable.Map

import java.net.InetSocketAddress
import java.util.concurrent.{TimeUnit, TimeoutException}

import net.lag.configgy.{Config, ConfigMap, Configgy, RuntimeEnvironment}
import net.lag.logging.Logger

import net.spy.memcached.{MemcachedClient, CachedData}
import net.spy.memcached.transcoders.Transcoder
import dispatch._
import dispatch.couch._
import dispatch.json._
import Js._

trait Task {
  /*  the task is executed using this method, which takes a json-unserialized map, and expects a map that can be serialized back*/
  /*  the given map will contain the keyword 'key' along with the key to the object. other elements will be given according to the task*/
  def run(params: Map[String, ExecutionProfile]): JsValue
  def identifier: String = { this.getClass.getName.toLowerCase }
  def version: String = "1"
}

trait Configurable {
  def setup(config: ConfigMap)
}

class JsonTranscoder extends Transcoder[JsValue] {
  def decode(d: CachedData) : JsValue = {
    var str = new String(d.getData.dropWhile(_ != '{')) // drop the first 4 bytes from ruby marshalling
    Logger.get.debug(str)
    Js(str)
  }
  
  def encode(v: JsValue): CachedData = {
    val bytes = v.toString.getBytes
    new CachedData(0, bytes, this.getMaxSize)
  }
  
  def getMaxSize = CachedData.MAX_SIZE
}

case class ExecutionProfile(val result: Option[JsValue], val duration: BigDecimal, val errors: List[String], val version: String) {
  def toJson = {
    val res = 'result << result.getOrElse(Js()) 
    val dur = 'duration << duration
    val errs = 'errors << errors
    val vers = 'version << version
    
    vers(res(dur(errs(Js()))))
  }
}

case object ExecutionProfile {
  def fromJson(js: JsObject): ExecutionProfile = {
    %('duration ! num, 'errors ! (list ! str), 'version ! str)(js) match {
      case (duration, errors, version) => ExecutionProfile(Some(js.self.apply(JsString("result"))), duration, errors, version)
    }
  }
}



case class Load(config: Config)

/*
  The queen will attempt to load a list of tasks from the configuration,
  create a worker for each of them and 
*/
object Queen extends Actor {
  
  var workers: List[Worker] = Nil
  var config: Config = null
  
  start
  
  def act = {
    loop {
      react {
        case Load(config) => load(config)
        case Exit(from: Worker, reason) => restart(from)
      }
    }
  }
  
  def load(config: Config): Unit = {
    this.config = config
    
    config.getConfigMap("chains") match {
      case Some(chainsConfig) => 
        workers = chainsConfig.keys.map { k => 
          Logger.get.info("Starting " + k + "...")
          val conf = chainsConfig.getConfigMap(k).get
          val tasksList = conf.getList("chain")
          val tasks = tasksList.map {t:String => loadTask(conf.getConfigMap(t).get)}
          val w = new Worker(config, tasks, k)
          w.start
          w
        }.toList
      case None => Nil
    }
  }
  
  def loadTask(conf: ConfigMap): Task = {
    val task = Class.forName(conf.getString("class", "")).newInstance.asInstanceOf[Task]
    task match {
      case t: Configurable => t.setup(conf)
      case _ => task
    }
    
    task
  }
  
  def restart(worker: Worker) = {
    
  }
}

object Bee {
  val runtime = new RuntimeEnvironment(getClass)
  var config: Config = null
  private val _startTime = System.currentTimeMillis
  
  def main(args: Array[String]): Unit = {
    runtime.load(args)
    startup(Configgy.config)
  }
  
  def startup(_config: Config): Unit = {
    config = _config
    
    Queen ! Load(config)
  }
}