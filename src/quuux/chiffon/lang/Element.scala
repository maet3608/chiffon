package quuux.chiffon.lang


/**
 * Marker value for elements with no value set
 */
object NOTHING {
  override def toString = "NOTHING"
}



/**
 * Element describes an element of the data flow and is the base class for items.
 */
abstract class Element {
  
  // whenever an element is created it is added to the dependency graph
  Dependencies.add(this)
  
  // registers a dependant of this element that gets updated when this elements value changes
  def -->(dependant:Element with Updateable) {
    Dependencies.read(this, dependant)
  }  
        
  // setter for a new internal value of an Element.
  def <<(newValue:Any):Unit 
    
  // getter for the internal value of an element. 
  def value:Any 
  
  // getter for the item assigned to the key.
  // must never return a new/different Element for the same key => otherwise dependency graph breaks
  // use Proxy if necessary
  def /(key:Element):Element
  
  override def toString = value.toString 
}





//class Error(src:Element, msg:String) extends Element {
//  override def get:Any = msg
//  override def /(key:Element):Element = this
//  override def toString = "Error: "+msg
//}
//
//object Error {
//  def apply(src:Element, msg:String) = new Error(src, msg)
//}


class Constant(_value:Any) extends Element { 
  def value = _value
  
  def <<(newValue:Any):Unit =
    throw new UnsupportedOperationException("<< not supported for : "+this.getClass.getName())
  
  def /(key:Element):Element = 
    throw new UnsupportedOperationException("/ not supported for : "+this.getClass.getName())
  
}

object Constant {
  def apply(value:Any) = new Constant(value) 
}



class Variable(var _value:Any) extends Constant(_value) {
  override def value = _value
  
  override def <<(newValue:Any):Unit = synchronized {
    val oldValue = _value
    _value = newValue
    val event = EventValueChanged(Variable.this, List(), oldValue, newValue)
    Dependencies.update(List(event))
  }

}

object Variable {
  def apply(value:Any) = new Variable(value)
}


object Conversions {
  
  implicit def any2variable(value:Any):Variable = Variable(value)  
  implicit def aa2variables(t:(Any,Any)) = (Variable(t._1), Variable(t._2))
  implicit def ae2variables(t:(Any,Element)) = (Variable(t._1), t._2)
  implicit def ea2variable(t:(Element, Any)) = (t._1, Variable(t._2))

//  implicit def pimpString(s:String): { def v: Value } = new { def v = Value(s) }
//  //implicit def pimpInt(i:Int): { def v: Element } = new { def v = Element(i) }
  
}


trait Updateable {
  // called when the value of an element this element depends on has changed
  def update(events:Iterable[Event]):Event   
}

trait Initializable extends Updateable {
  // called when the dependency graph is initialized to initialize the element value
  // typically used with class Function
  def initialize():Unit   
}
 

abstract class Function(var _value:Any) extends Constant(_value) with Initializable {    
  override def value = _value  
}


abstract class ProxyMap(var _value:Any) extends Constant(_value) with Updateable { 
  override def value = _value
  
  // maps a key value to its element. Used by Proxy 
  // Note the difference to def /(key:Element):Element
  def /(value:Any):Element
}



class Proxy(key:Element, dir:ProxyMap) extends Function(NOTHING)  {
  key --> this    // => call update when key changes
  dir --> this    // => call update when directory changes

  /** Returns the element from the directory for the given key */
  private def directory(k:Element):Element = dir / (k.value)
   

  def initialize() {
    _value = directory(key).value
  }

  def update(events:Iterable[Event]):Event = {
    val oldValue = _value
    _value = directory(key).value
    EventValueChanged(this, events, oldValue, value)
  }

  override def /(k:Element):Element = {
    directory(key) / k
  }

  override def <<(newValue:Any):Unit = synchronized {
    directory(key) << newValue
  }
  
  override def toString = directory(key).toString
}




class Directory extends ProxyMap(Map[Element,Element]()) {
  private var elem2proxy = Map[Element,Proxy]()     // key to proxy of element
  private var value2elem = Map[Any,Element]()       // key.value to element  
  private def elem2elem = _value.asInstanceOf[Map[Element,Element]]   // key to element

  def add(k:Element,v:Element) {
    _value = elem2elem + (k->v)
    value2elem += k.value -> v
    k --> this    // => call update when added key changes
    v --> this    // => call update when added value changes
  }

  override def /(k:Element):Element = {
    if(!elem2proxy.contains(k))
      elem2proxy += (k->new Proxy(k, this))
    elem2proxy(k)
  }
  
  def /(value:Any):Element = value2elem(value)
  

  // called when keys or values change
  def update(events:Iterable[Event]):Event = {
    def isKey(e:Element) = elem2elem.keySet.contains(e)
    def updateKey(oldKey:Any,newKey:Any) {
      val value = value2elem(oldKey)
      value2elem -= oldKey
      value2elem += (newKey -> value)
    }
    events.collect{case EventValueChanged(src,_,oldValue,newValue) => if(isKey(src)) updateKey(oldValue,newValue)}
    EventDirChanged(this,events)
  }

  override def toString = elem2elem.map{case (k,v) => k+":"+v}.mkString("{",", ","}")
}

object Directory {
  def apply(entries:(Element,Element)*):Directory = {
    val dir = new Directory()
    for((k,v) <- entries)
      dir.add(k,v)
    dir
  }
//  def apply(map:Map[_,_]):Directory = {
//    val dir = new Directory()
//    for((k,v) <- map) {
//      v match {
//        case v:Map[_,_] => dir.add(Variable(k), Directory(v))
//        case _ => dir.add(Variable(k),Variable(v))  
//      }
//    }
//    dir
//  }
}


object Vector {
  def apply(entries:Element*) = {
    val dir = new Directory()
    for((v,i) <- entries.zipWithIndex)
      dir.add(Constant(i),v)
    dir
  }
}


object Ticker extends Variable(Map[Element,Element]()) {
  def elem2elem = value.asInstanceOf[Map[Element,Element]]   // key to element

  override def /(k:Element):Element = {
    if(!elem2elem.contains(k))
      _value = elem2elem + (k->new Tick(k))
    elem2elem(k)
  }
}


class Tick(k:Element) extends Function(0) {
  import java.util.{Timer, TimerTask}

  k --> this    // => call update when key changes

  var counter = 0
  var timer:Timer = _

  def rate = k.value.asInstanceOf[Int]  // tick rate

  def newTimer = {
    var timer = new Timer()
    val task = new TimerTask() {
      override def run() {
        Tick.this << counter
        counter += 1
      }
    }
    timer.scheduleAtFixedRate(task, 0, rate)
    timer
  }

  override def update(events:Iterable[Event]):Event = {
    println("updating tick", rate, events)
    val oldTimer = timer
    timer.cancel()
    timer = newTimer
    EventValueChanged(this,events, oldTimer, timer)
  }

  def initialize() {
    println("tick init",rate)
    timer = newTimer
  }
}


class Files(path:String) extends Constant(path) {
  import java.io.File
  private val file = new File(path)
  private var elem2elem = Map[Element,Element]()

  override def /(k:Element):Element = {
    if(k == Files.SIZE) return Variable(file.length)
    if(k == Files.ISDIR) return Variable(file.isDirectory)
    if(!elem2elem.contains(k))
      elem2elem = elem2elem + (k->new Files(path+"/"+k.value.toString))
    elem2elem(k)
  }
}

object Files {
  val SIZE = new Variable("SIZE")
  val ISDIR = new Variable("ISDIR")
  def apply(path:String) = new Files(path)
}


