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
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureExprFactory}
import de.fosd.typechef.featureexpr.FeatureExprFactory._
import org.scalatest._
import org.scalatest.matchers.{MatchResult, BeMatcher}

/** Tests that various regular expressions features are correctly implemented
 *  by testing matches against strings.
 *
 *  @author Lucas Satabin
 */
class VMatchingTest extends FlatSpec with ShouldMatchers {

  val fa = FeatureExprFactory.createDefinedExternal("A")
  val fb = FeatureExprFactory.createDefinedExternal("B")

  class EquivMatcher(expected:FeatureExpr) extends BeMatcher[FeatureExpr] {
    def apply(left: FeatureExpr) =
      MatchResult(
        left equivalentTo expected,
        left.toString + " was equivalent to expected",
        left.toString + " was not equivalent to "+expected.toString()
      )
  }
  def equiv(f:FeatureExpr) = new EquivMatcher(f)

  "A single character regular expression" should "match iff the string is this character" in {

    val re = "a".vre

    re.isMatchedBy(ChoiceStr(fa, "a", "b")) shouldBe equiv(fa)
    re.isMatchedBy(ChoiceStr(fa, "a", "a")) shouldBe equiv(True)

  }

  it should "not match a string containing many times this character" in {

    val re = "a".vre

    re.isMatchedBy(ChoiceStr(fa, "a", "aaa")) shouldBe equiv(fa)
    re.isMatchedBy(ChoiceStr(fa, "aaa", "aaa")) shouldBe equiv(False)

  }

  it should "not match a string containing another character" in {

    val re = "a".vre

    re.isMatchedBy(ChoiceStr(fa, "ab", "a")) shouldBe equiv(fa.not)

  }



  "Alternative" should "match if at least one possibility matches the string" in {

    val re = "ab|ac|ad|a.".vre

    re.isMatchedBy(List(Opt(True,'a'), Opt(fa, 'b'), Opt(fa.not,'c'))) shouldBe equiv(True)
    re.isMatchedBy(List(Opt(True,'a'), Opt(fa, 'd'), Opt(fa.not,'e'))) shouldBe equiv(True)
    re.isMatchedBy(List(Opt(True,'a'), Opt(fa, 'd'))) shouldBe equiv(fa)
    re.isMatchedBy(List(Opt(True,'a'), Opt(fa, 'd'), Opt(fb,'e'))) shouldBe equiv(fa xor fb)

  }

  it should "not match if the string does not matche any possibility" in {

    val re = "ab|ac|ad|a.".vre

    re.isMatchedBy(AString("abad")) shouldBe equiv(False)

  }

  "Variability" should "work with repeatition" in {

    val re = "[ab]*".vre

    re.isMatchedBy(Concatenate((True,"aaaaaa"), (fa, "c"),(fb, "b"),(True, "aabbaa"))) shouldBe equiv(fa.not)

  }


  "Character set" should "match if the string is contained in this set" in {

    val re = "[a-zA-Z_][a-z[A-Z]\\d_]*".vre

    re.isMatchedBy(Concatenate((True,"some_iden"), (fa, " "),(True, "tifier43"))) shouldBe equiv(fa.not)

  }

  it should "not match if at a character is not in the set" in {

    val re = "[a-zA-Z_][a-zA-Z0-9_]*".vre

    re.isMatchedBy("98toto") shouldBe equiv(False)
    re.isMatchedBy("tété") shouldBe equiv(False)

  }
//
//  "Negated character set" should "match any character not in the set" in {
//
//    val re = "[^a-z]+".vre
//
//    re.isMatchedBy("+98_") shouldBe equiv(true)
//
//  }
//
//  it should "not match character in the set" in {
//
//    val re1 = "[^a-z]+".vre
//    val re2 = "\\D+".vre
//    val re3 = "\\W+".vre
//    val re4 = "\\S+".vre
//
//    re1.isMatchedBy("a_") shouldBe equiv(false)
//    re2.isMatchedBy("23") shouldBe equiv(false)
//    re3.isMatchedBy("a_") shouldBe equiv(false)
//    re4.isMatchedBy(" ") shouldBe equiv(false)
//
//  }
//
//  "Optional character" should "match if present" in {
//
//    val re = "a?".vre
//
//    re.isMatchedBy("a") shouldBe equiv(true)
//
//  }
//
//  it should "match if not present" in {
//
//    val re = "a?".vre
//
//    re.isMatchedBy("") shouldBe equiv(true)
//
//  }
//
//  it should "not match if some other character is present" in {
//
//    val re = "a?".vre
//
//    re.isMatchedBy("b") shouldBe equiv(false)
//
//  }
//
//  "Starred character" should "match if present once" in {
//
//    val re = "a*".vre
//
//    re.isMatchedBy("a") shouldBe equiv(true)
//
//  }
//
//  it should "match if present several times" in {
//
//    val re = "a*".vre
//
//    re.isMatchedBy("aaaaaaaaaaaaaa") shouldBe equiv(true)
//
//  }
//
//  it should "match if not present" in {
//
//    val re = "a*".vre
//
//    re.isMatchedBy("") shouldBe equiv(true)
//
//  }
//
//  it should "not match if at least one other character is present" in {
//
//    val re = "a*".vre
//
//    re.isMatchedBy("aaaaabaaaaaa") shouldBe equiv(false)
//
//  }
//
//  "Plus character" should "match if present once" in {
//
//    val re = "a+".vre
//
//    re.isMatchedBy("a") shouldBe equiv(true)
//
//  }
//
//  it should "match if present several times" in {
//
//    val re = "a+".vre
//
//    re.isMatchedBy("aaaaaaaaaaaaaa") shouldBe equiv(true)
//
//  }
//
//  it should "not match if not present" in {
//
//    val re = "a+".vre
//
//    re.isMatchedBy("") shouldBe equiv(false)
//
//  }
//
//  it should "not match if at least one other character is present" in {
//
//    val re = "a+".vre
//
//    re.isMatchedBy("aaaaabaaaaaa") shouldBe equiv(false)
//
//  }

}

