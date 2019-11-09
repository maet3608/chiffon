package quuux.chiffon.lang


// maps elements to the input events they receive during update
// this way an element knows, which of its inputs has changed

abstract class Event(val src:Element, val events:Iterable[Event])
case class EventValueChanged(override val src:Element, override val events:Iterable[Event], oldValue:Any, newValue:Any) extends Event(src,events)
case class EventDirChanged(override val src:Element, override val events:Iterable[Event]) extends Event(src,events)



class EventMap {
  val map = scala.collection.mutable.Map[Element,List[Event]]()

  def add(event:Event) {
    for(reader <- Dependencies.toNode(event.src).outputs) {
      if(map.contains(reader.element))
        map(reader.element) = event::map(reader.element)
      else
        map += (reader.element -> List(event))
    }
  }

  def add(events:Iterable[Event]) {
    events.foreach(add)
  }

  def get(node:DNode) = {
    map.getOrElse(node.element, List())
  }
}
