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
  def run(params: JsValue): JsValue
}

trait Configurable {
  def setup(config: ConfigMap)
}

class JsonTranscoder extends Transcoder[JsValue] {
  def decode(d: CachedData) : JsValue = {
    var str = new String(d.getData.dropWhile(_ != '{')) // drop the first 4 bytes from ruby marshalling
    Console.println(str)
    Logger.get.debug(str)
    Js(str)
  }
  
  def encode(v: JsValue): CachedData = {
    val bytes = v.toString.getBytes
    new CachedData(0, bytes, this.getMaxSize)
  }
  
  def getMaxSize = CachedData.MAX_SIZE
}


/* 
 * A worker waits for a task from the queue and executes it, storing the result in the nest
 * The queue identifies the type of work, and the data contained will be stored as json
 */
class Worker(val config: Config, val task: Task, taskName: String) extends Actor {
  //link(Queen)
  def act = {
    trapExit = true
    
    val addr = new InetSocketAddress(config.getString("queue.host", "localhost"), config.getInt("queue.port", 22133))
    val c = new MemcachedClient(addr);
    val transcoder = new JsonTranscoder
        
    loop {
      val future = c.asyncGet(taskName + "/t=" + config.getInt("wait", 10 * 1000), transcoder)
      
      try {
        val json = future.get(12, TimeUnit.SECONDS)
        
        if(json != null) {
          /* extracts the key from the document */
          val keyExtractor = 'key ? str
          val keyExtractor(key) = json

          Hive ! Store(key, task.run(json), taskName)
        }
      } catch {
          case e:TimeoutException => future.cancel(true);
      }
    }
  }
}

/* The nest will provide the interface for storing and querying the CouchDB instance */

case class Store(val key: String, val data: JsValue, taskName: String)
object Hive extends Actor {
  //link(Queen)
  val http = new Http
  val couch = new Couch(Bee.config.getString("couchdb.host", "localhost"),
                        Bee.config.getInt("couchdb.port", 5984))
                    
  val db = Db(couch, Bee.config.getString("couchdb.database", "bee"))
  http.x(db) { (code, _, _) => if(code == 404) { http(db create) } }
  
  start
  
  def act = {
    trapExit = true
    
    loop {
      react {
        case Store(key, data, task) => 
          val doc = Doc(db, key)
          val docData = if(http.x(doc) { (code, _, _) => code == 404}) {
            Js()
          }
          else
          {
            http(doc >> { stm => json.Js(stm)})
          }
          val newData = tag(merge(docData, task, data), task)
          put(doc, newData)
      }
    }
  }
  
  def merge(docData: JsValue, task: String, data: JsValue): JsValue = {
    (Symbol(task) << data)(docData)
  }
  
  def tag(docData: JsValue, tag: String): JsValue = {
    val tagsEx = 'tags ? list
    /* TODO: check if the tags is already there */
    val tags = JsString(tag) :: (docData match {
                  case tagsEx(tags) => tags
                  case _ => Nil
                })
    ('tags << tags)(docData)
  }
  
  def put(document: Doc, newContent: JsValue): Unit = {
    http(document.update(newContent))
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
        case Exit(from, reason) => restart(from.asInstanceOf[Worker])
      }
    }
  }
  
  def load(config: Config): Unit = {
    this.config = config
    
    config.getConfigMap("tasks") match {
      case Some(tasksConfig) => 
        workers = tasksConfig.keys.map { k => 
          Console.println("Starting " + k + "...")
          val task = Class.forName(tasksConfig.configMap(k).getString("class", "")).newInstance.asInstanceOf[Task]
          task match {
            case t: Configurable => t.setup(tasksConfig.configMap(k))
            case _ => task
          }
          new Worker(config, task, k)}.toList
        for(worker <- workers) {
          worker.start
        }
        workers
      case None => Nil
    }
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