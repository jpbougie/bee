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


/* 
 * A worker waits for a task, or a list of tasks from the queue and executes it, storing the result in the nest
 * The queue identifies the type of work, and the data contained will be stored as json
 */
class Worker(val config: Config, val tasks: Seq[Task], queueName: String) extends Actor {

  def act = {    
    val addr = new InetSocketAddress(config.getString("queue.host", "localhost"), config.getInt("queue.port", 22133))
    val c = new MemcachedClient(addr);
    val transcoder = new JsonTranscoder
        
    loop {
      
      val future = c.asyncGet(queueName + "/t=" + config.getInt("wait", 10 * 1000), transcoder)
      
      try {
        val request = future.get(12, TimeUnit.SECONDS)
        
        if(request != null) {
          /* extracts the key from the document */
          val keyExtractor = 'key ? str
          val keyExtractor(key) = request
          
          /* fetches the previously stored document if it exists */
          val previousDocumentF = Hive !! Get(queueName, key)
          
          /* takes the JSON object and transform it into a map[String, ExecutionProfile] */
          val previousDocument = Map.empty ++ previousDocumentF().asInstanceOf[Option[JsObject]].map {
                                              o =>
                                                o.self.map { case (JsString(k), v) => k -> ExecutionProfile.fromJson(v.asInstanceOf[JsObject]) }
                                              }.getOrElse(Map.empty)
          
          /* check if the input is really the same, start over otherwise */
          val initial = if(previousDocument.contains("input") && previousDocument("input").result == Some(request)) {
            previousDocument
          } else {
            Map("input" -> ExecutionProfile(Some(request), 0, Nil, "1"))
          }
          /* 
           * Executes each task in the chain, storing the results as an execution profile 
           * in the map that will be stored. The first element is always going to be called input,
           * and will contain the parameters that were received from the queue.
           */
          val map: Map[String, ExecutionProfile] = tasks.foldLeft(initial) {  (data, task) =>
            
            /* no need to re-run an already successfully completed task */
            if(data.contains(task.identifier) && 
               data(task.identifier).errors == Nil && 
               data(task.identifier).version == task.version) {
              data
            } else {
              val startTime = System.currentTimeMillis
              val output = try {
                val result = task.run(data)
                (Some(result), Nil)
              } 
              catch {
                case e : Exception => (None, e.toString :: Nil)
              }
              val elapsedTime = System.currentTimeMillis - startTime

              /* add the new data to the map */
              data + (task.identifier -> ExecutionProfile(output._1, elapsedTime, output._2, task.version))
            }
            
          }
          
          Hive ! Store(queueName, key, map)
        }
      } catch {
          case e:TimeoutException => future.cancel(true);
          //case e => Logger.get.error("Worker encountered an exception, %s", e.toString)
      }
    }
  }
}