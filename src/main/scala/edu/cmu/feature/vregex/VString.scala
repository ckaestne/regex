package edu.cmu.feature.vregex

import de.fosd.typechef.conditional.{One, Choice, Conditional, Opt}
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.featureexpr.FeatureExprFactory.True

trait VString {
    def toVCharList: List[Opt[Char]] = _toVCharList(True)
    protected[vregex] def _toVCharList(expr: FeatureExpr): List[Opt[Char]]
}

case class Concatenate(l: List[VString]) extends VString {
    protected[vregex] def _toVCharList(f: FeatureExpr): List[Opt[Char]] = l.foldLeft(List[Opt[Char]]())(_ ++ _._toVCharList(f))
}

object Concatenate {
//    def apply(s: String*) = new Concatenate(s.map(s=>AString(s)).toList)
    def apply(s: (FeatureExpr, String)*) = new Concatenate(s.map(s=>ChoiceStr(s._1,s._2,"")).toList)
}

case class ChoiceStr(l: Conditional[VString]) extends VString {
    protected[vregex] def _toVCharList(f: FeatureExpr): List[Opt[Char]] =
        l.toList.flatMap({case (e,s)=>s._toVCharList(f and e)})

}

object ChoiceStr {
    def apply(f: FeatureExpr, a:String, b: String): VString = new ChoiceStr(Choice(f, One(AString(a)), One(AString(b))))
}

//atomic string
case class AString(s: String) extends VString {
    protected[vregex] def _toVCharList(f: FeatureExpr): List[Opt[Char]] = s.toCharArray.toList.map(Opt(f, _))

}
