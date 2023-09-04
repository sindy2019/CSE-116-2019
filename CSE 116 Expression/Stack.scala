package expression


class Stack[T] {
  var ptr : LinkedListNode[T] = null
  def push(value : T) = {
    ptr= new LinkedListNode[T](value, ptr)
  }

  def pop() = {
    var tmp = ptr.value
    ptr = ptr.next
    tmp
  }

  def top() : T = {
    ptr.value
  }

  def isEmpty() = {
    ptr == null
  }
}
