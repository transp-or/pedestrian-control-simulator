import com.typesafe.config.Config
import hubmodel.parseConfigFile
import optimization.simulation.{runFlowSepFunction, runGatingSingleFunction}

object OptimizeControlStrategy extends App {

  // ******************************************************************************************
  //                    Read CLI arguments and process parameters file
  // ******************************************************************************************

  // Reads the file passed as argument
  val config: Config = parseConfigFile(args)


  // ******************************************************************************************
  //                    Optimization of the control strategy
  // ******************************************************************************************


  def f = runGatingSingleFunction(config)(_,_,_,_,_)

  val params = Vector(
    (0.0,-2.0,0.0,0.0,0.0)
  )

  /*def f = runFlowSepFunction(config)(_,_,_,_,_)

  val params = Vector(
    (0.5,0.0,0.0,0.0,0.0),
    (1.0,4.0,0.0,0.0,-1.0),
    (1.0,1.0,0.0,0.0,-1.0),
    (1.0,1.0,0.0,0.0,-2.0),
    (1.0,0.25,0.0,0.0,-1.0)
  )*/

  val res = for (p <- params) yield {
    f(p._1, p._2, p._3, p._4, p._5)
  }

  res.foreach(println)

}