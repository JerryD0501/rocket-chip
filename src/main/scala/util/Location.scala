// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import scala.language.dynamics
import scala.collection.mutable.Map

class Location[T](val name: String)

class LocationMap[T] private(lm: Map[String, T]) extends Map[Location[T], T] with Dynamic {
  private val internalMap = lm
  def +=(kv: (Location[T], T)) = new LocationMap[T](internalMap + (kv._1.name -> kv._2)).asInstanceOf[this.type]
  def -=(key: Location[T]) = new LocationMap(internalMap - key.name).asInstanceOf[this.type]
  def get(key: Location[T]) = internalMap.get(key.name)
  def iterator = internalMap.iterator.map(kv => (new Location(kv._1), kv._2))

  def select(location: Location[T]): T = selectDynamic(location.name)
  def selectDynamic(name: String): T = {
    internalMap.getOrElse(name, throw new Exception(s"could not find location named $name"))
  }

  def update(location: Location[T])(value: T): Unit = updateDynamic(location.name)(value)
  def updateDynamic(name: String)(value: T): Unit = {
    internalMap += name -> value
  }
}

object LocationMap {
  def apply[T](lm: Map[String, T]): LocationMap[T] = new LocationMap(lm)
  def empty[T]: LocationMap[T] = new LocationMap(Map.empty[String, T])
}
