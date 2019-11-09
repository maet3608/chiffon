package quuux.chiffon.sys

import quuux.chiffon.lang._

/**
 * 
 */
class Display() extends Function(NOTHING) {

  def initialize() {
    _value = Dependencies.inputs(this).map(_.toString).mkString(" ")
    println(_value)
  }

  def update(events:Iterable[Event]):Event = {
    val oldValue = value
    initialize()
    EventValueChanged(this,events,oldValue,value)
  }

}

object Display {
  def apply() =  new Display
  def apply(elements:Element*) = {
    val display = new Display
    elements foreach (_ --> display)  
    display
  }
}