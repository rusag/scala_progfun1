type Set = Int => Boolean

def contains(s: Set, elem: Int): Boolean = s(elem)

/**
  * Returns the set of the one given element.
  */
def singletonSet(elem: Int): Set = (x :Int) => x == elem


contains(singletonSet(1), 2)