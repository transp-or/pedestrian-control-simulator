package optimization.ALNS

import hubmodel.control.{ControlDeviceData, ControlDevicePolicy}
import hubmodel.prediction.state.StateGroundTruthPredicted

import scala.annotation.tailrec
import scala.util.Random

/** Container for the control device policy.
  *
  * The equality has been defined correctly for this container, therefore this class can be used as keys in maps.
  *
  * @param x control device policy
  */
class Policy(val x: Vector[ControlDevicePolicy]) {

  private def stringify: String = this.x.sortBy(p => (p.name, p.start)).map(dv => dv.nameToString + (10.0 * dv.decisionVariable).round.toInt.toString).mkString("-")


  /** Checks whether another object equals this one
    *
    * @param other another object to test equality for
    * @return boolean indicating if the two objects are the same
    */
  override def equals(other: Any): Boolean = {
    other match {
      case that: Policy => that.canEqual(this) && this.stringify == that.stringify
      case _ => false
    }
  }

  /** Checks whether we are allowed to compare this object to another
    *
    * @param other other element to compare to
    * @return true if the comparison is authorized
    */
  def canEqual(other: Any): Boolean = {
    other.isInstanceOf[Policy]
  }


  /** Definition of equality.
    *
    * @return Int representing the object
    */
  override def hashCode: Int = {
    this.stringify.##
  }
}

/**
  *
  */
trait ParetoSet {

  /** Function to reduce multiple evalautions of the objective function to a scalar.
    *
    * @return reduction of the vector of values
    */
  def stochasticReduction: FunctionEvaluation => FunctionEvaluationReduced

  // collection of pareto solutions
  protected val paretoSet: collection.mutable.Map[Policy, (Vector[ControlDeviceData], FunctionEvaluation, Vector[StateGroundTruthPredicted])] = collection.mutable.Map()

  /** Selection of one solution with the set to use for the generation of new solutions.
    *
    * Currently this selects randomly a solution
    *
    * @return
    */
  def selectSolution: Policy = {
    Random.shuffle(this.paretoSet.toVector).head._1
  }

  /**
    *
    * @param x           policy to insert
    * @param controlData associated control data
    * @param ofs         objective functions after function evalutation
    * @param stateData   state data for solution x
    * @return string indicating whether the solution was accepted or rejected
    */
  def insert(x: Policy, controlData: Vector[ControlDeviceData], ofs: FunctionEvaluation, stateData: Vector[StateGroundTruthPredicted]): String = {

    /**
      *
      * @param dominatedBy x is dominated by these solutions
      * @param dominating  x is dominating these soluations
      * @param solutions   set of solutions to check
      * @return the set of dominatedBy and dominating solutions
      */
    @tailrec def helper(dominatedBy: Vector[Policy],
                        dominating: Vector[Policy],
                        solutions: List[Policy],
                        controlData: Vector[ControlDeviceData],
                        ofs: FunctionEvaluation,
                        stateData: Vector[StateGroundTruthPredicted]): (Vector[Policy], Vector[Policy]) = {
      if (solutions.isEmpty) { // if the set of pareto solution is empty, then return the dominated and dominating sets
        (dominatedBy, dominating)
      } else { // otherwise extract the first element from the pareto set and start processing it
        val that :: solutionsTail = solutions

        if (that == x) { // if the solution to insert is the same as an already existing solution, then update it.

          paretoSet.update(
            x,
            (controlData,
              (this.paretoSet.getOrElse(x, (controlData, Map(), Vector()))._2.toVector ++ ofs.toVector).groupBy(_._1).view.mapValues(v => v.flatMap(_._2)).toMap,
              this.paretoSet.getOrElse(x, (controlData, Map(), Vector()))._3 ++ stateData
            )
          )

          // after updating the solution, we must restart the insertion process as the solution has moved.
          val updatedSolution: (Vector[ControlDeviceData], FunctionEvaluation, Vector[StateGroundTruthPredicted]) = this.paretoSet.remove(x).get

          //insert(x, updatedSolution._1, updatedSolution._2, updatedSolution._3)
          helper(Vector(), Vector(), this.paretoSet.keys.toList, updatedSolution._1, updatedSolution._2, updatedSolution._3)

        } else if (thisDominatesThat(stochasticReduction(ofs), stochasticReduction(this.paretoSet(that)._2))) {
          helper(dominatedBy, dominating :+ that, solutionsTail, controlData, ofs, stateData)
        } else if (thisDominatesThat(stochasticReduction(this.paretoSet(that)._2), stochasticReduction(ofs))) {
          helper(dominatedBy :+ that, dominating, solutionsTail, controlData, ofs, stateData)
        } else {
          helper(dominatedBy, dominating, solutionsTail, controlData, ofs, stateData)
        }
      }
    }

    // calls the helper function to insert the new solution in the pareto set
    val (dominatedBy, dominating) = helper(Vector(), Vector(), this.paretoSet.keys.toList, controlData, ofs, stateData)

    // if x is dominated by nothing, then add x and remove all solutions that x is dominating
    if (dominatedBy.isEmpty) {
      dominating.foreach(dom => this.paretoSet.remove(dom))
      this.paretoSet.addOne((x, (controlData, ofs, stateData)))
      "accepted"
    } else {
      "rejected"
    }
  }

  /** Returns true if thisSol dominates thatSol, false otherwise.
    * Each element of thisSol must be stricly smaller than it's counterpart in thatSol.
    *
    * @param thisSol this solution to compare with
    * @param thatSol that other solution
    * @return
    */
  private def thisDominatesThat(thisSol: FunctionEvaluationReduced, thatSol: FunctionEvaluationReduced): Boolean = {
    !thisSol.exists(thisOF => thisOF._2 > thatSol(thisOF._1))
  }

}
