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

/* The hive will provide the interface for storing and querying the CouchDB instance */
case class Store(val queueName: String, val key: String, val data: Map[String, ExecutionProfile])
case class Get(val queueName: String, val key: String)
object Hive extends Actor {
  val http = new Http
  val couch = new Couch(Bee.config.getString("couchdb.host", "localhost"),
                        Bee.config.getInt("couchdb.port", 5984))
                    
  val db = Db(couch, Bee.config.getString("couchdb.database", "bee"))
  
  /* create the db if it doesn't exist */
  http.x(db) { (code, _, _) => if(code == 404) { http(db create) } }
  
  start
  
  def act = {
    trapExit = true
    
    loop {
      react {
        case Store(queue, key, data) => 
          val doc = Doc(db, key)
          val docData = http.x(doc) { 
              case (404, _, _ ) => Js()
              case (status, req, None) => Js()
              case (status, req, Some(ent)) => Js(ent.getContent)
            }
          val newData = merge(docData, queue, data)
          put(doc, newData)
        case Get(queue, key) =>
          val doc = Doc(db, key)
          reply(http(doc >> { stm => json.Js(stm)}))
      }
    }
  }
  
  def merge(docData: JsValue, task: String, data: Map[String, ExecutionProfile]): JsValue = {
    (Symbol(task) << (data.transform { (k, v) => v.toJson }))(docData)
  }
  
  def put(document: Doc, newContent: JsValue): Unit = {
    http(document.update(newContent))
  }
}
