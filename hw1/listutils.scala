object listutils extends App {
  def length[T](list: List[T]): Int = {
    def lengthHelper(list: List[T], accum: Int): Int = {
      if (list == Nil)
        accum
      else
        lengthHelper(list.tail, accum + 1)
    }
    lengthHelper(list, accum = 0)
  }

  def find[T](item: T, list: List[T]): Int = {
    def findHelper(item: T, list: List[T], accum: Int): Int = {
      if (list == Nil)
        -1
      else if (list.head == item)
        accum
      else
        findHelper(item, list.tail, accum + 1)
    }
    findHelper(item, list, accum = 0)
  }

  def reverse[T](list: List[T]): List[T] = {
    def reverseHelper(list: List[T], accum: List[T]): List[T] = {
      if (list == Nil)
        accum
      else {
        reverseHelper(list.tail, list.head :: accum)
      }
    }
    reverseHelper(list, accum = List())
  }

  def map[T, U](fn: T => U, list: List[T]): List[U] = {
    def mapHelper(fn: T => U, list: List[T], accum: List[U]): List[U] = {
      if (list == Nil)
        accum
      else
        mapHelper(fn, list.tail, fn(list.head) :: accum)
    }
    mapHelper(fn, reverse(list), accum = List())
  }

  def filter[T](fn: T => Boolean, list: List[T]): List[T] = {
    def filterHelper(fn: T => Boolean, list: List[T], accum: List[T]): List[T] = {
      if (list == Nil)
        accum
      else {
        if (fn(list.head))
          filterHelper(fn, list.tail, list.head :: accum)
        else
          filterHelper(fn, list.tail, accum)
      }
    }
    filterHelper(fn, reverse(list), accum = List())
  }

  def reduce[T](fn: (T, T) => T, list: List[T]): T = {
    def reduceHelper(fn: (T, T) => T, list: List[T], accum: T): T = {
      if (list == Nil)
        accum
      else {
        reduceHelper(fn, list.tail, fn(accum, list.head))
      }
    }

    reduceHelper(fn, list.tail, accum = list(0))
  }

  def concat[T](list1: List[T], list2: List[T]): List[T] = {
    def concatHelper(list1: List[T], list2: List[T], accum: List[T]): List[T] = {
      if (list2 == Nil) {
        if (list1 == Nil) {
          accum
        } else {
          concatHelper(list1.tail, list2, list1.head :: accum)
        }
      } else {
        concatHelper(list1, list2.tail, list2.head :: accum)
      }
    }
    concatHelper(reverse(list1), reverse(list2), accum = List())
  }

  def merge[T](list1: List[T], list2: List[T])(implicit ordering: Ordering[T]): List[T] = {
    import ordering.mkOrderingOps
    def mergeHelper(list1: List[T], list2: List[T], accum: List[T])(implicit ordering: Ordering[T]): List[T] = {
      if ((list1 == Nil) && (list2 == Nil))
        accum
      else {
        if (list1 == Nil)
          mergeHelper(list1, list2.tail, list2.head :: accum)
        else if (list2 == Nil)
          mergeHelper(list1.tail, list2, list1.head :: accum)
        else if (list1.head >= list2.head)
          mergeHelper(list1.tail, list2, list1.head :: accum)
        else
          mergeHelper(list1, list2.tail, list2.head :: accum)
      }
    }
    mergeHelper(reverse(list1), reverse(list2), accum = List())
  }

  def mergeSort[T](list: List[T])(implicit ordering: Ordering[T]): List[T] = {
    if (list == Nil)
      list
    else if (list.tail == Nil)
      list
    else {
      merge((mergeSort(list.take(list.length / 2))), mergeSort(list.drop(list.length / 2)))
    }
  }
}