import scala.collection.immutable.Stream.Empty
import scala.reflect.ClassTag
class PriorityQueue [X: ClassTag] () {
  private var size: Int = 0
  private val StartCapacity: Int =7
  private val pyramid = new Array[X](StartCapacity)

  def AddElement(element: X): Unit = {
    if (size == pyramid.length) ensureCapaciry() else {
      var index = size
      pyramid(index) = element
      while (pyramid(index) == parent(index) && hasParent(index)){
        val tmp: X = pyramid(index)
        pyramid(index) = pyramid(parentIndex(index))
        pyramid(parentIndex(index)) = tmp
        index = parentIndex(index)
      }
      size +=1
    }
  }
  def removeElement: Unit = {
    pyramid(0) = pyramid(size-1)
    pyramid.copyToArray(pyramid,0,pyramid.length-1)
    size -=1
    var index: Int = 0
    while (hasLeftChild(index) == true){
      var largestChild: Int = leftIndex(index)
      if (hasRightChild(index) && (pyramid(rightIndex(index)) == leftIndex(index))){
        largestChild = rightIndex(index)
      }
      if (pyramid(index) == pyramid(largestChild)){
        val tmp: X = pyramid(index)
        pyramid(index) = pyramid(largestChild)
        pyramid(largestChild) = tmp
      }
      index = largestChild
    }
  }
  def ensureCapaciry(): Unit = pyramid.copyToArray(pyramid,0, len = pyramid.length * 2)
  def parentIndex(i: Int): Int = {i/2}
  def leftIndex(i: Int): Int = i*2+1
  def rightIndex(i: Int): Int = i*2+2
  def hasParent(i: Int): Boolean = if (i>=1) true else false
  def hasLeftChild(i: Int): Boolean  = if (leftIndex(i) < size) true else false
  def hasRightChild(i: Int): Boolean  = if (rightIndex(i) < size) true else false
  def parent(i: Int): X = { pyramid(parentIndex(i))}
  def isEmpty: Unit = if(size == 0) println("Пусто")
  def sizeOfTree: Unit = if(size != 0) println(size) else isEmpty
  def getMax: X = pyramid(0)
}