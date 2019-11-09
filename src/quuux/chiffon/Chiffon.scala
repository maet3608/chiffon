package quuux.chiffon

import lang._
import math._
import sys._


/**
 * Created with IntelliJ IDEA.
 * User: s.maetschke
 * Date: 29/01/13
 * Time: 1:44 PM
 * To change this template use File | Settings | File Templates.
 */
object Chiffon  {
  def main(args:Array[String]) {
  import Conversions._

  println("running...")
  val e0 = Variable(0)
  val e1 = Variable(1)
  val e2 = Variable(2)
  //val sum = Sum(e0,e1)
  //Display("sum:",sum)
  val display = Display()
  val sum = Sum()
  e1 --> sum
  e2 --> sum
  "sum = " --> display
  sum --> display
  

  def Mean(a:Element, b:Element):Element = Div(Sum(a,b), Constant(2))
  //val display = Display()
  //"mean =" --> display
  //Mean(e1,e2) --> display
  Display("mean =", Mean(e1,e2))


  val cond = Variable(true)
  val True = Constant("TRUE")
  val False = Constant("FALSE")
  val IF = new Directory()
  IF.add(true, True)
  IF.add(false, False)
  Display("Condition:", IF / cond )

  val contacts = Directory("Albert" -> 
                   Directory("Firstname" -> "Albert", "Age" -> 20, "Address" -> 
                    Directory("Street" -> 
                       Directory("Name" -> "Queen", "Number" -> 81))))
//  val contacts2 = Directory(Map("Albert"->
//                      Map("Firstname" -> "Albert", "Age" -> 20, "Address" -> 
//                          Map("Street" -> 
//                             Map("Name" -> "Queen", "Number" -> 81)))))                                                 
  val vectorVar = Variable("two")
  val vector = Vector("zero", "one", vectorVar)
  val matrix = Vector(
    Vector(1,2,3),
    Vector(4,5,6))

  val key = Variable("Name")
  val who = Variable("Albert")   
  Display("Street", key, "=", contacts/who/"Address"/"Street"/key )
  Display("contact =", contacts )
  Display("Vector/2 =", vector/2 )
  Display("matrix/1/2 =", matrix/1/2)
  Display("matrix/1 =", matrix/1)
  

//  val tick1 = Ticker/1000
//  //val tick2 = Ticker/1000
//  Display("tick",Sum(tick1, tick1))
  val num = Variable(1)
  Display("3*num =",Sum(num,num,num))

  val c_drive = new Files("c:")
  Display("size of readme.txt:",c_drive/"Maet"/"Temp"/"Signatures.txt"/Files.SIZE)
  Display("isDir temp:",c_drive/"Maet"/"Temp"/Files.ISDIR)

  println("-----------Init---------------------")
  Dependencies.initialize()
  println("-----------Update-------------------")
  e1 << 10
  //key << "Number"
  cond << false
  //False << "CANT DO" 
  num << 10
  (matrix/1/2) << 10 
  vectorVar << "three"
  (contacts/"Albert"/"Address"/"Street"/"Name") << "Goolman"


  println("finished.")
  }
}