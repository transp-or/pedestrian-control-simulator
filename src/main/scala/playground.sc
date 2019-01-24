import hubmodel.ped.PedestrianSim
import hubmodel.supply.graph.RouteGraph

/*case class MyCell(center: Position, edgeLength: Double){ //}, conn: List[String]) {
  val ID: String = generateUUID
  val A: Position = center + edgeLength*DenseVector(-cos(30*math.Pi/180.0), sqrt(1-pow(cos(30*math.Pi/180.0),2)))
  val B: Position = A + edgeLength*DenseVector(0.0,-1.0)
  val C: Position = B + edgeLength*DenseVector(cos(30.0*math.Pi/180.0), -sqrt(1.0-pow(cos(30.0*math.Pi/180.0),2)))
  val D: Position = C + edgeLength*DenseVector(cos(30.0*math.Pi/180.0), sqrt(1.0-pow(cos(30.0*math.Pi/180.0),2)))
  val E: Position = D + edgeLength*DenseVector(0.0,1.0)
  val F: Position = E + edgeLength*DenseVector(-cos(30.0*math.Pi/180.0), sqrt(1.0-pow(cos(30.0*math.Pi/180.0),2)))

  val angles: List[Position] = List(A,B,C,D,E,F)

  var pedAcc: Double = 0.0
  var potential: Double = 0.0
  var stepsToFinal: Int = 0
  var updateState: Int = 0


  def isInside(p: Position): Boolean = {
    // https://stackoverflow.com/questions/5193331/is-a-point-inside-regular-HexagonPotentialField
    val d: Double = breeze.linalg.norm(p - center)

    if (d > edgeLength) return false
    else if (d <= edgeLength*cos(30.0*math.Pi/180.0)) return true

    val px: Double = (p(0)-center(0)) * 2/sqrt(3)
    if (px > 1.0 || px < -1.0) return false

    val py: Double = 0.5 * px + (p(1) - center(1))
    if (py < 1.0 || py < -1.0 ) false
    else if (px-py < 1.0 || px-py < -1.0) false
    else true
  }

  def xCoords: Array[Double] = Array(A(0), B(0), C(0), D(0), E(0), F(0))
  def yCoords: Array[Double] = Array(A(1), B(1), C(1), D(1), E(1), F(1))

}
*/
/*object MyVertex {
  implicit def orderingByPotential[A <: MyCell]: Ordering[A] = Ordering.by(v => v.potential)
}
*/
/*
case class MyConnection(v: HexagonPotentialField, c: List[HexagonPotentialField])
object MyConnection {
  implicit def orderingByPotential[A <: MyConnection]: Ordering[A] = Ordering.by(c => c.v.potential)
}
*/
/*
val a = new HexagonPotentialField(new Position(0.0,0.0), 2.0)//, List("b","c","d"))
val b = new HexagonPotentialField(new Position(0.0,0.0), 2.0)//, List("a","e"))
val c = new HexagonPotentialField(new Position(0.0,0.0), 2.0)//, List("a","d","h","f","g"))
val d = new HexagonPotentialField(new Position(0.0,0.0), 2.0)//, List("a","i","c","h"))
val e = new HexagonPotentialField(new Position(0.0,0.0), 2.0)//, List("b"))
val f = new HexagonPotentialField(new Position(0.0,0.0), 2.0)//, List("c","g"))
val g = new HexagonPotentialField(new Position(0.0,0.0), 2.0)//, List("c","f"))
val h = new HexagonPotentialField(new Position(0.0,0.0), 2.0)//, List("i","d","c"))
val i = new HexagonPotentialField(new Position(0.0,0.0), 2.0)//, List("d","h"))
val j = new HexagonPotentialField(new Position(0.0,0.0), 2.0)//, List("d","h"))
val k = new HexagonPotentialField(new Position(0.0,0.0), 2.0)//, List("d","h"))
val l = new HexagonPotentialField(new Position(0.0,0.0), 2.0)//, List("d","h"))
val m = new HexagonPotentialField(new Position(0.0,0.0), 2.0)//, List("d","h"))
val n = new HexagonPotentialField(new Position(0.0,0.0), 2.0)//, List("d","h"))
val o = new HexagonPotentialField(new Position(0.0,0.0), 2.0)//, List("d","h"))
val p = new HexagonPotentialField(new Position(0.0,0.0), 2.0)//, List("d","h"))
val q = new HexagonPotentialField(new Position(0.0,0.0), 2.0)//, List("d","h"))
val r = new HexagonPotentialField(new Position(0.0,0.0), 2.0)//, List("d","h"))
val s = new HexagonPotentialField(new Position(0.0,0.0), 2.0)//, List("d","h"))
val t = new HexagonPotentialField(new Position(0.0,0.0), 2.0)//, List("d","h"))
val u = new HexagonPotentialField(new Position(0.0,0.0), 2.0)//, List("d","h"))
val v = new HexagonPotentialField(new Position(0.0,0.0), 2.0)//, List("d","h"))
val w = new HexagonPotentialField(new Position(0.0,0.0), 2.0)//, List("d","h"))
val x = new HexagonPotentialField(new Position(0.0,0.0), 2.0)//, List("d","h"))*/
//val destination = new HexagonPotentialField(new Position(0.0,0.0), 2.0)

/*
f.pedAcc=3.0
g.pedAcc=3.0
c.pedAcc=3.0
r.pedAcc=4.0
s.pedAcc=2.0
a.pedAcc=5.0


val connections: List[MyConnection] = List(
 MyConnection(a,List(b,c,d)),
  MyConnection(b, List(a,e)),
  MyConnection(c, List(a,d,h,f,g)),
  MyConnection(d, List(a,e,i,c,h)),
  MyConnection(e, List(b)),
  MyConnection(f, List(c,g)),
  MyConnection(g, List(c,f)),
  MyConnection(h, List(i,d,c)),
  MyConnection(i, List(d,h))
)*/
/*
val connections2: Map[HexagonPotentialField, List[HexagonPotentialField]] = Map(
  a -> List(b,c,d),
  b -> List(a,e),
  c -> List(a,d,h,f,g),
  d -> List(a,e,i,c,h),
  e -> List(b,x),
  f -> List(c,g,r),
  g -> List(c,f,r),
  h -> List(i,d,c),
  i -> List(d,h,j,k),
  j -> List(h,i,k,l),
  k -> List(i,j,l),
  l -> List(j,k,m),
  m -> List(l,n),
  n -> List(m,o),
  o -> List(n,p),
  p -> List(o,q),
  q -> List(p,r,s),
  r -> List(f,g,s,q),
  s -> List(q,r,t),
  t -> List(s,u),
  u -> List(t,v),
  v -> List(u,w),
  w -> List(x,v),
  x -> List(e,w)
)*/
/*
val xMin = 0.0
val xMax = 100.0
val yMin = 0.0
val yMax = 20.0
val radius: Double = 2


def insideSpace(p: Position): Boolean = {
  val xMin1 = 0.0
  val xMax1 = 10.0
  val yMin1 = 0.0
  val yMax1 = 7.0


  val xMin2 = 0.0
  val xMax2 = 100.0
  val yMin2 = 7.0
  val yMax2 = 13.0
  (p.X >= xMin1 && p.X <=xMax1 && p.Y >= yMin1 && p.Y <= yMax1) || (p.X >= xMin2 && p.X <=xMax2 && p.Y >= yMin2 && p.Y <= yMax2)
}


val HexagonPotentialFields: IndexedSeq[HexagonPotentialField] = (for (
  x <- xMin to xMax by 2*radius*cos(30.0*math.Pi/180.0);
  y <- yMin to yMax by 3*radius)
  yield {
    new HexagonPotentialField(new Position(x,y), radius)
  }).filter(h => h.corners.exists(insideSpace)) ++ (for (
  x <- (xMin+radius*cos(30.0*math.Pi/180.0)) to xMax by 2*radius*cos(30.0*math.Pi/180.0);
  y <- yMin+1.5*radius to yMax by 3*radius)
  yield {
    new HexagonPotentialField(new Position(x,y), radius)
  }).filter(h => h.corners.exists(insideSpace))



val connections2: Map[HexagonPotentialField, List[HexagonPotentialField]] = HexagonPotentialFields.map(h => h -> HexagonPotentialFields.filter(hin => (h.center - hin.center).norm < 1.01*2*radius*cos(30.0*math.Pi/180.0)).filterNot(h == _).toList).toMap


val doorwayPoints = (9.0 to 11.0 by 0.25).map(y => new Position(0.0,y))

val finalCells: IndexedSeq[HexagonPotentialField] = HexagonPotentialFields.filter(h => doorwayPoints.exists(h.isInside))

*/

// static algortihm
/*
// block 1
finalCells.foreach(_.stepsToFinal=1)
var l: Int = 1
while (connections.exists(_.v.stepsToFinal==0)){
  connections.filter(_.v.stepsToFinal == l).foreach(_.c.filter(c => c.stepsToFinal==0).foreach(c => c.stepsToFinal=l+1))
  l = l + 1
}

// block 2
val lMax: Int = connections.map(_.v.stepsToFinal).max
val theta: Double = 0.8
connections.filter(_.v.stepsToFinal==1).foreach(_.v.potential=1.0)
l = 2
while(l <= lMax) {
  connections.filter(_.v.stepsToFinal == l).foreach(p => {
    val setCells: List[HexagonPotentialField] = p.c.filter(c => c.potential >= 0.0 && c.stepsToFinal == l - 1)
    if (setCells.size == 1) {
      p.v.potential = setCells.head.potential + 1.0
    }
    else {
      p.v.potential = theta + setCells.map(_.potential).sum / setCells.size.toDouble
    }
  })
  l = l + 1
}

println(connections.map(_.v.potential))
*/
/*
// dynamic algorithm
val tau: Double = 0.1
val theta: Double = 0.9
var mCounter = finalCells.length
//var l = 1
finalCells.foreach(v => {v.updateState = 1; v.potential = 1})
var lCounter: Int = 1
while (mCounter != 0) {
  val V = connections2.filter(v => v._1.updateState == 1 && v._1.potential <= lCounter)
  V.foreach(_._1.updateState = 2)
  mCounter = mCounter - V.size
  V.foreach( conn => {
    conn._2.filter(_.updateState == 0).foreach(j => {
      j.updateState = 1
      mCounter = mCounter + 1
      val psi: Int = connections2(j).count(_.updateState == 2)
      if (psi == 1) {
        j.potential = conn._1.potential + 1 + tau * j.pedAcc
      }
      else {
        j.potential = connections2(j).filter(_.updateState==2).map(_.potential).sum / psi + theta + tau * j.pedAcc
      }
    })
    lCounter = lCounter + 1
  })
}

println(lCounter, connections2.map(conn => conn._1.ID + " pot=" + conn._1.potential).mkString("\n"))
//println(u.potential,v.potential,w.potential,x.potential,e.potential,b.potential,a.potential)
//println(u.potential,t.potential,s.potential,r.potential,f.potential,c.potential,a.potential)

val destination = new HexagonPotentialField(new Position(0.0,0.0), radius)
val conn3: Map[HexagonPotentialField, List[HexagonPotentialField]] = connections2 + (destination -> finalCells.toList)

def buildGraph(conn: (HexagonPotentialField, List[HexagonPotentialField]), connections: Map[HexagonPotentialField, List[HexagonPotentialField]], acc: List[(HexagonPotentialField, HexagonPotentialField)]): List[(HexagonPotentialField, HexagonPotentialField)] = {
  if (connections.isEmpty) acc ++ conn._2.filter(_.potential >= conn._1.potential).map((_, conn._1))
  else buildGraph(connections.head, connections.tail, conn._2.filter(_.potential >= conn._1.potential).map((_, conn._1)) ++ acc)
}

val g = buildGraph(conn3.head, conn3.tail, List())

println(HexagonPotentialFields.map(_.potential).mkString("\n"))

new DrawCells(HexagonPotentialFields, "celltest.png")//, None, (xMax, yMax))
*/
/*

import scala.math._
import scala.util.parsing.combinator._
import scala.util.Random

class FormulaParser(val constants: Map[String,Double] = Map(), val userFcts: Map[String,String => Double] = Map(), random: Random = new Random) extends JavaTokenParsers {
  require(constants.keySet.intersect(userFcts.keySet).isEmpty)
  private val allConstants = constants ++ Map("E" -> E, "PI" -> Pi, "Pi" -> Pi) // shouldn´t be empty
  private val unaryOps: Map[String,Double => Double] = Map(
    "sqrt" -> (sqrt(_)), "abs" -> (abs(_)), "floor" -> (floor(_)), "ceil" -> (ceil(_)), "ln" -> (math.log(_)), "round" -> (round(_)), "signum" -> (signum(_))
  )
  private val binaryOps1: Map[String,(Double,Double) => Double] = Map(
    "+" -> (_+_), "-" -> (_-_), "*" -> (_*_), "/" -> (_/_), "^" -> (pow(_,_))
  )
  private val binaryOps2: Map[String,(Double,Double) => Double] = Map(
    "max" -> (max(_,_)), "min" -> (min(_,_))
  )
  private def fold(d: Double, l: List[~[String,Double]]) = l.foldLeft(d){ case (d1,op~d2) => binaryOps1(op)(d1,d2) }
  private implicit def map2Parser[V](m: Map[String,V]) = m.keys.map(_ ^^ (identity)).reduceLeft(_ | _)
  private def expression:  Parser[Double] = sign~term~rep(("+"|"-")~term) ^^ { case s~t~l => fold(s * t,l) }
  private def sign:        Parser[Double] = opt("+" | "-") ^^ { case None => 1; case Some("+") => 1; case Some("-") => -1 }
  private def term:        Parser[Double] = longFactor~rep(("*"|"/")~longFactor) ^^ { case d~l => fold(d,l) }
  private def longFactor:  Parser[Double] = shortFactor~rep("^"~shortFactor) ^^ { case d~l => fold(d,l) }
  private def shortFactor: Parser[Double] = fpn | sign~(constant | rnd | unaryFct | binaryFct | userFct | "("~>expression<~")") ^^ { case s~x => s * x }
  private def constant:    Parser[Double] = allConstants ^^ (allConstants(_))
  private def rnd:         Parser[Double] = "rnd"~>"("~>fpn~","~fpn<~")" ^^ { case x~_~y => require(y > x); x + (y-x) * random.nextDouble } | "rnd" ^^ { _ => random.nextDouble }
  private def fpn:         Parser[Double] = floatingPointNumber ^^ (_.toDouble)
  private def unaryFct:    Parser[Double] = unaryOps~"("~expression~")" ^^ { case op~_~d~_ => unaryOps(op)(d) }
  private def binaryFct:   Parser[Double] = binaryOps2~"("~expression~","~expression~")" ^^ { case op~_~d1~_~d2~_ => binaryOps2(op)(d1,d2) }
  private def userFct:     Parser[Double] = userFcts~"("~(expression ^^ (_.toString) | ident)<~")" ^^ { case fct~_~x => userFcts(fct)(x) }
  def evaluate(formula: String) = parseAll(expression,formula).get
}

val formulaParser = new FormulaParser(
  constants = Map("radius" -> 8D,
    "height" -> 10D,
    "c" -> 299792458, // m/s
    "v" -> 130 * 1000 / 60 / 60, // 130 km/h in m/s
    "m" -> 80),
  userFcts  = Map("perimeter" -> { _.toDouble * 2 * Pi } ))

println(formulaParser.evaluate("2+3*5x")) // 17.0
println(formulaParser.evaluate("height*perimeter(radius)")) // 502.6548245743669
println(formulaParser.evaluate("m/sqrt(1-v^2/c^2)"))  // 80.00000000003415
*/


val t1 = (1,2,3,4)


t1.productIterator.toVector
val t2 = (7,8,9)
