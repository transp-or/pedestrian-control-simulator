package hubmodel.demand

import java.util.concurrent.ThreadLocalRandom

import hubmodel.ped.PedestrianNOMAD

class PTInducedQueue() {

  private val _queue: collection.mutable.ArrayBuffer[CreatePedestrian] = collection.mutable.ArrayBuffer()

  var rate: Double = 1.8 // ped/s for a width of 2.7m.

  def isEmpty: Boolean = this._queue.isEmpty

  def nonEmpty: Boolean = this._queue.nonEmpty

  def appendPeds(peds: Iterable[CreatePedestrian]): Unit = {
    this._queue.appendAll(peds)
  }

  def samplePed: CreatePedestrian = this._queue.remove(ThreadLocalRandom.current.nextInt(this._queue.size))

}
