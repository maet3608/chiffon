package quuux.chiffon.lang



/**
 * Dependency graph and its nodes. Defines which elements are updated in which order during an update cycle.
 * Also defines the inputs and outputs of an element.
 */
class DNode(val element:Element, var outputs:List[DNode], var inputs:List[DNode], var level:Int) {
  def isInitializable = element.isInstanceOf[Initializable]
  def initialize() = element.asInstanceOf[Initializable].initialize() 
  def update(events:Iterable[Event]):Event = element.asInstanceOf[Updateable].update(events)
  override def toString = element+":"+level
}


object DNode {
  def apply(element:Element,outputs:List[DNode], inputs:List[DNode], level:Int) =
    new DNode(element,outputs,inputs,level)
}



object Dependencies {
  private var maxLevel = 0

  // maps element to node
  var toNode = Map[Element,DNode]()

  def inputs(element:Element):List[Element] = toNode(element).inputs.reverse.map(_.element)


  // TODO: what about cycles?
  private def setLevels(nodes:Iterable[DNode], level:Int)  {
    if(nodes.isEmpty) return
    nodes.foreach{n => n.level = if(n.inputs.isEmpty) 0 else n.inputs.map(_.level).max + 1}
    setLevels(nodes.map(_.outputs).flatten, level+1)
  }

  def add(element:Element) {
    toNode += element -> DNode(element,List(),List(),0)
  }

  def read(parameter:Element, dependant:Element with Updateable) {
    toNode(parameter).outputs ::= toNode(dependant)
    toNode(dependant).inputs ::= toNode(parameter)
    setLevels(toNode.values.filter(_.inputs.length == 0), 0)
    maxLevel = toNode.values.map(_.level).max
  }

  /** all sub-nodes of the given nodes.  */
  private def subNodes(nodes:Iterable[DNode]):List[DNode] = {
    var visited = Set[DNode]()
    def collect(node:DNode) {
      if(visited.contains(node)) return  // don't traverse same node twice
      visited += node
      node.outputs.foreach(collect)
    }
    nodes.foreach(collect)
    visited.toList
  }

  def initialize() {
    val nodes = toNode.values.filter(_.isInitializable).groupBy(_.level)
    for (level <- 0 to maxLevel if nodes.contains(level))
      nodes(level).par.foreach(_.initialize())
  }

  def update(events:Iterable[Event]) =  synchronized {
    val nodes = subNodes(events.map(e => toNode(e.src))).groupBy(_.level)
    val eventMap = new EventMap()
    eventMap.add(events)
    for (level <- 1 to maxLevel if nodes.contains(level))  {
      val newEvents = nodes(level).par.map(n => n.update(eventMap.get(n))).toList
      eventMap.add(newEvents)
    }
  }
}