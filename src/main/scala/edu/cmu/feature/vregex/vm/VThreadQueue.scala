/*
* This file is part of the regex project.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package edu.cmu.feature.vregex.vm

import scala.collection.immutable.Queue

/** An immutable queue that maintains a hashset of the contained thread's pc.
 *  to allow for quick existence check.
 *
 *  @author Lucas Satabin
 */
class VThreadQueue(val threads: Queue[VThread]) {

  def enqueue(newt: VThread): VThreadQueue = {

    var foundPC = false;
    var q = threads.map(t => if (t.pc == newt.pc && t.matched==newt.matched) {foundPC =true; VThread(t.startIdx, t.pc, t.ctx or newt.ctx, t.saved)} else t)
    if (!foundPC)
      q=q.enqueue(newt)
    new VThreadQueue(q)

  }

//  def dequeue: (VThread, VThreadQueue) = {
//    val (thread, rest) = threads.dequeue
//    (thread, new VThreadQueue(rest, pcs - thread.pc))
//  }

//  def isEmpty: Boolean =
//    threads.isEmpty
//
//  def nonEmpty: Boolean =
//    threads.nonEmpty
//
//  def contains(t: VThread): Boolean =
//    pcs.contains(t.pc)

  def hasUnmatched: Boolean = threads.filterNot(_.isMatched).nonEmpty
  def matched = threads.filter(_.isMatched)
}

object VThreadQueue {

  def apply(): VThreadQueue =
    new VThreadQueue(Queue())

}

