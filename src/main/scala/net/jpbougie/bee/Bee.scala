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

import net.lag.configgy.{Config, ConfigMap, Configgy, RuntimeEnvironment}
import net.lag.logging.Logger

import net.spy.memcached.MemcachedClient
import dispatch._
import dispatch.couch._
import dispatch.json._
import Js._

trait Task {
  /*  the task is executed using this method, which takes a json-unserialized map, and expects a map that can be serialized back*/
  /*  the given map will contain the keyword 'key' along with the key to the object. other elements will be given according to the task*/
  def run(params: JsValue): JsValue
  /* identifies the task uniquely, used to find the good queue */
  def identifier: String 
}


/* 
 * A worker waits for a task from the queue and executes it, storing the result in the nest
 * The queue identifies the type of work, and the data contained will be stored as json
 */
class Worker(val config: Config, val task: Task) extends Actor {
  //link(Queen)
  def act = {
    trapExit = true
    
    val addr = new InetSocketAddress(config.getString("memcache.host", "localhost"), config.getInt("memcache.port", 22133))
    val c = new MemcachedClient(addr);
        
    loop {
      val returned = c.get(task.identifier + "/t=" + config.getInt("wait", 10 * 1000).toString).asInstanceOf[String]
      if(returned != null) {
        val result = Js()

        /* extracts the key from the document */
        val keyExtractor = 'key ? str
        val keyExtractor(key) = result

        Hive ! Store(key, task.run(result), task)
      } else {
        Thread.sleep(10 * 1000)
      }
      
    }
    
  }
}

/* The nest will provide the interface for storing and querying the CouchDB instance */

case class Store(val key: String, val data: JsValue, task: Task)
object Hive extends Actor {
  //link(Queen)
  val http = new Http
  val couch = new Couch(Bee.config.getString("couchdb.host", "localhost"),
                        Bee.config.getInt("couchdb.port", 5984))
                    
  val db = Db(couch, Bee.config.getString("couchdb.database", "bee"))
  http(db create)
  
  start
  
  def act = {
    trapExit = true
    
    loop {
      react {
        case Store(key, data, task) => 
          val doc = Doc(db, key)
          val docData = http(doc >> { stm => json.Js(stm)})
          val newData = tag(merge(docData, task, data), task.identifier)
          put(doc, newData)
      }
    }
  }
  
  def merge(docData: JsValue, task: Task, data: JsValue): JsValue = {
    (Symbol(task.identifier) << data)(docData)
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
        workers = tasksConfig.keys.map { k => new Worker(config, Class.forName(tasksConfig.configMap(k).getString("class", "")).newInstance.asInstanceOf[Task])}.toList
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