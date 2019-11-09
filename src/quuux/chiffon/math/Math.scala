package quuux.chiffon.math

import quuux.chiffon.lang._


class Sum() extends Function(0) {

  def initialize() {
    _value = Dependencies.inputs(this).map(_.value).collect{case n:Int => n}.sum
  }

  def update(events:Iterable[Event]):Event = {
    val oldValue = value.asInstanceOf[Int]
    //value = Dependencies.inputs(this).map(_.value).collect{case n:Int => n}.sum
    _value = oldValue + events.collect{case EventValueChanged(_,_,ov:Int,nv:Int) => nv-ov}.sum
    EventValueChanged(this,events,oldValue,value)
  }

}

object Sum {
  def apply(elements:Element*) = {
    val sum = new Sum
    elements foreach (_ --> sum)  
    sum
  }
}



class Div() extends Function(0.0) {

  def initialize() {
    val inputs = Dependencies.inputs(this).map(_.value).collect{case n:Int => n}.toSeq
    _value = inputs(0)/inputs(1).toDouble
  }

  def update(events:Iterable[Event]):Event = {
    val oldValue = value.asInstanceOf[Double]
    initialize()
    EventValueChanged(this,events,oldValue,value)
  }
  //override def toString = "Sum("+value.toString+")"
}

object Div {
  def apply(elements:Element*) = {
    val div = new Div
    elements foreach (_ --> div)  
    div
  }
}