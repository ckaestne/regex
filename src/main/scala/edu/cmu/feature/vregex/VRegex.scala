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
package edu.cmu.feature.vregex

import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.featureexpr.FeatureExpr
import edu.cmu.feature.vregex.vm.VVM
import gnieh.regex.compiler._
import gnieh.regex.util._
import de.fosd.typechef.featureexpr.FeatureExprFactory._


class VRegex(re: ReNode, source: Option[String]) extends Serializable {

  def this(source: String) =
    this(Parser.parse(source).get, Some(source))

  private lazy val (saved, compiled) = Compiler.compile(re)

  //println(util.Debug.print(compiled))
  def isMatchedBy(input: String): Boolean = isMatchedBy(AString(input).toVCharList).isTautology()
  def isMatchedBy(input: VString): FeatureExpr = isMatchedBy(input.toVCharList)

  /** Tells whether this regular expression is matched by the given input */
  def isMatchedBy(input: List[Opt[Char]]): FeatureExpr = {
    val r = VVM.exec(compiled, saved, 0, True, input)
    r.when({case (start, end, _) => start == 0 && end == input.length})
  }

//  /** Finds the first match of this regular expression in the input.
//   *  If nothing matches, returns `None`*/
//  def findFirstIn(input: String): Option[String] =
//    for {
//      m <- findFirstMatchIn(input)
//      matched <- m.matched
//    } yield matched
//
//  /** Finds the first match of this regular expression in the input.
//   *  If nothing matches, returns `None`*/
//  def findFirstMatchIn(input: String): Option[Match] = {
//    def find(startIdx: Int): Option[Match] =
//      VM.exec(compiled, saved, startIdx, input) match {
//        case (-1, -1, _) if startIdx < input.size =>
//          find(startIdx + 1)
//        case (-1, -1, _) =>
//          None
//        case (start, end, groups) =>
//          Some(new Match(start, end, groups, input))
//      }
//    find(0)
//  }
//
//  /** Finds all matches of this regular expression in the input. */
//  def findAllIn(input: String): Iterator[String] =
//    for {
//      m <- findAllMatchIn(input)
//      matched <- m.matched
//    } yield matched
//
//  /** Finds all matches of this regular expression in the input. */
//  def findAllMatchIn(input: String): Iterator[Match] = {
//    def loop(startIdx: Int): Stream[Match] =
//      VM.exec(compiled, saved, startIdx, input) match {
//        case (-1, -1, _) if startIdx < input.size =>
//          loop(startIdx + 1)
//        case (-1, -1, _) =>
//          Stream.empty
//        case (start, end, groups) =>
//          val m = new Match(start, end, groups, input)
//          if(start == end && end == input.size)
//            // this is an empty match and we reach the end of the input
//            // just return this match and stop
//            Stream(m)
//          else
//            m #:: loop(end)
//      }
//    loop(0).iterator
//  }
//
//  def unapplySeq(input: String): Option[List[String]] =
//    for {
//      m @ Match(start, end) <- findFirstMatchIn(input)
//      if start == 0 && end == input.size
//    } yield m.subgroups

  override def toString =
    source.getOrElse(re.toString)

}

object VRegex {

  def apply(str: String): VRegex =
    new VRegex(str)

}

