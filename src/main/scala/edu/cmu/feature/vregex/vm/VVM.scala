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

import de.fosd.typechef.conditional.{Choice, One, Conditional, Opt}
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.featureexpr.FeatureExprFactory._
import edu.cmu.feature.vregex.VString
import gnieh.regex.util._

import scala.annotation.tailrec

/** A virtual machine executing the regular expression code against some input.
 *  A run on some input returns the first match if any. It is stateless, thus executing the same
 *  regular expression against the same input will always return the same result.
 *
 *  This is a thread safe, non-backtracking, tail-recursive implementation allowing
 *  for efficient stack-less executions.
 *
 *  @author Lucas Satabin
 */
object VVM {

  /** Executes the given regular expression program with the given string input.
   *  It returns a lazily constructed streamm of all matches in the input.
   */
  def exec(program: Vector[Inst], nbSaved: Int, startIdx: Int, ctx: FeatureExpr, string: List[Opt[Char]]):  Conditional[(Int, Int, Vector[Int])] = {
    //println("executing:")
    //println(util.Debug.print(program))
    //println(s"with input: $string")

    val emptySaved = Vector.fill(nbSaved * 2)(-1)

    def createResult(matchedThreads:VThreadQueue): Conditional[(Int, Int, Vector[Int])] = {
      var result:Conditional[(Int, Int, Vector[Int])] = One((-1,-1,emptySaved))
      for (thread <- matchedThreads.matched) {
        result = Choice(thread.ctx, One((thread.startIdx, thread.matched, thread.saved)), result)
      }
      return result
    }

    @tailrec
    def eloop(idx: Int, threads: VThreadQueue): Conditional[(Int, Int, Vector[Int])] = {
      val res =
        if(idx >= string.size)
          step(program, nbSaved, string.length, None, threads)
        else {
          step(program, nbSaved, idx, Some(string(idx)), threads)
        }

      if (res.hasUnmatched && idx < string.size)
        eloop(idx + 1, res)
      else
        createResult(res)

//      res match {
//        case Next(threads) =>
//          if(threads.isEmpty)
//            // did not match
//            (lastStart, lastEnd, lastSaved)
//          else
//            eloop(idx + 1, threads, lastStart, lastEnd, lastSaved)
//        case Matched(start, end, saved, threads) if threads.nonEmpty && idx < string.size =>
//          // a match was found but if some higher priority threads still exist
//          // try if we find a longer match
//          eloop(idx + 1, threads, start, end, saved)
//        case Matched(start, end, saved, threads) =>
//          (start, end, saved)
//      }
    }
    // create and schedule the first thread in which the vritual machine executes the code
    eloop(
      startIdx,
      schedule(program, VThread(startIdx, 0, ctx, emptySaved), VThreadQueue(), startIdx))
  }

  /* given the list of current thread and the currently inspected character, execute one step */
  private def step(program: Vector[Inst], nbSaved: Int, idx: Int, optChar: Option[Opt[Char]], threads: VThreadQueue): VThreadQueue = {
    var result = VThreadQueue()
    val charctx = optChar.map(_.feature).getOrElse(True)
    val char = optChar.map(_.entry)
    for (thread <- threads.threads)
      if (thread.isMatched) result.enqueue(thread) // matched threads are not further processed
    else  {
      val VThread(startIdx, pc, ctx, saved, _) = thread
      val sidx=if(startIdx >= 0) startIdx else idx

      def skipOrProcess(vthread: Option[VThread]) {
        val skip = (ctx and charctx).isContradiction()
        val matchesCompletely = !skip && (ctx implies charctx).isTautology()
        val split = !matchesCompletely && !skip

        if (skip)
        //reschedule without changes
          result= schedule(program, thread, result, idx)
        else if (matchesCompletely) {
          //normal progress
          if (vthread.isDefined)
            result = schedule(program, vthread.get, result , idx + 1)
        } else {
          //schedule two VThreads with restricted presence conditions
          result = schedule(program, thread andNot charctx, result , idx)
          if (vthread.isDefined)
            result=schedule(program, vthread.get and charctx, result , idx+1)
        }
      }

      //println(s"at index: $idx with char: $char")
      //println(s"threads: $threads")
      fetch(program, pc) match {
        case AnyMatch() =>
          // any characters matches
          skipOrProcess(  Some(VThread(sidx, pc + 1, ctx, saved) ))

        case CharMatch(c) if char == Some(c) =>
          // the current character matches the expected one, just try the next thread and save the
          // next instruction in this thread for the next step
          skipOrProcess( Some(VThread(sidx, pc + 1, ctx, saved)))
        case CharMatch(_) =>
          // the current character does not match the expected one, discard this thread
          skipOrProcess(None)
        case ClassMatch(tree) if char.isDefined && tree.contains(char.get) =>
          // the current character is in the expected class, schedule the next instruction in this thread and try further
          skipOrProcess( Some(VThread(sidx, pc + 1, ctx, saved)))
        case ClassMatch(_) =>
          // the current character is not is the expected range, discard this thread
          skipOrProcess(None)
        case MatchFound =>
          // a match was found
          skipOrProcess(Some(VThread(startIdx, pc, ctx, saved, idx)))
//          Matched(startIdx, idx, saved, acc)
      }
    }
    return result
  }

  private def fetch(program: Vector[Inst], pc: Int): Inst =
    if(pc >= 0 && pc < program.size)
      program(pc)
    else
      throw new RuntimeException("Invalid regular expression")

  private def schedule(program: Vector[Inst], thread: VThread, queue: VThreadQueue, idx: Int): VThreadQueue =
    if (thread.isMatched)
      queue.enqueue(thread)
    else {
      fetch(program, thread.pc) match {
        case Split(i1, i2) =>
          schedule(
            program,
            VThread(thread.startIdx, i2, thread.ctx, thread.saved),
            schedule(
              program,
              VThread(thread.startIdx, i1, thread.ctx, thread.saved),
              queue,
              idx),
            idx)
        case Jump(i) =>
          schedule(program, VThread(thread.startIdx, i, thread.ctx,thread.saved), queue, idx)
        case Save(n) =>
          schedule(program, VThread(thread.startIdx, thread.pc + 1, thread.ctx, thread.saved.updated(n, idx)), queue, idx)
        case _ =>
          queue.enqueue(thread)
      }
    }

}

final case class VThread(startIdx: Int, pc: Int, ctx: FeatureExpr, saved: Vector[Int], matched: Int= -1) {
  def and(f: FeatureExpr) = VThread(startIdx, pc, ctx and f, saved, matched)
  def andNot(f: FeatureExpr) = VThread(startIdx, pc, ctx andNot f, saved, matched)
  def isMatched = matched >= 0
}

sealed trait StepResult
final case class Matched(start: Int, end: Int, saved: Vector[Int], threads: VThreadQueue) extends StepResult
final case class Next(threads: VThreadQueue) extends StepResult

