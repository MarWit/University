def scalarUgly(xs: List[Int], ys: List[Int]) = {
  var pairs = xs zip ys
  var output = 0;
  do {
    val (x, y) = pairs.head
    output += x * y
    pairs = pairs.tail
  } while(!pairs.isEmpty)
  output
}

def scalar(xs: List[Int], ys: List[Int]) = {
  val output = for((x, y) <- (xs zip ys)) yield x * y
  output.sum
}

def sortUgly(xs: List[Int]): List[Int] =
  if(xs.length <= 1) xs
  else {
    var (left, middle, right) = (List[Int](), List[Int](), List[Int]())
    var l = xs
    val middle_el = xs(xs.length / 2)

    do {
      val item = l.head
      if(item < middle_el) left = item :: left
      else if(item > middle_el) right = item :: right
      else middle = item :: middle
      l = l.tail
    } while(!l.isEmpty)

    sortUgly(left) ::: middle ::: sortUgly(right)
  }


def sort(xs: List[Int]): List[Int] =
  if(xs.length <= 1) xs
  else {
    val middle_el = xs(xs.length / 2)
    val left = for(x <- xs if x < middle_el) yield x
    val right = for(x <- xs if x > middle_el) yield x
    val middle = for(x <- xs if x == middle_el) yield x
    sort(left) ::: middle ::: sort(right)
  }

def isPrimeUgly(n: Int): Boolean = {
  if(n <= 3) { return n > 1 }
  if((n % 2) == 0 || (n % 3) == 0) { return false }
  var i = 5
  while(i * i <= n) {
    if((n % i) == 0 || (n % (i+2)) == 0) {
      return false
    }
    i += 6
  }
  true
}

def isPrime(n: Int): Boolean =
  if(n <= 3) n > 1
  else if((n % 2) == 0 || (n % 3) == 0) false
  else {
    for(i <- LazyList.from(5).map(x => x*x).takeWhile(_ <= n) if ((n % i) == 0 || (n % (i+2)) == 0))
      return false
    true
  }

def primePairsUgly(n : Int): List[(Int, Int)] = {
  var output = List[(Int, Int)]()
  var i = 1
  do {
    var j = 1
    do {
      j += 1
      if(isPrime(i+j)) {
        output = (i, j) :: output
      }
    } while(j < i)
    i += 1
  } while(i < n)
  output.reverse
}

def primePairs(n : Int): List[(Int, Int)] = {
  val pairs = for(i <- 1 to n;
                  j <- 1 to i - 1;
                  pair = (i, j) if isPrime(i + j)) yield pair
  pairs.toList
}

val filesHere = new java.io.File(".").listFiles

def fileLinesUgly(file: java.io.File): List[String] = {
  var lines = List[String]()
  val scanner = new java.util.Scanner(file)
  do {
    lines = scanner.nextLine() :: lines
  } while(scanner.hasNext())
  lines.reverse
}

def fileLines(file: java.io.File): List[String] =
  scala.io.Source.fromFile(file).getLines.toList

def printNonEmptyUgly(): Unit = {
  var files = filesHere
  do {
    val content = fileLines(files.head)
    if(!content.isEmpty)
      println(files.head)
    files = files.tail
  } while(files.nonEmpty)
}

def printNonEmpty(): Unit =
  for(file <- filesHere if !fileLines(file).isEmpty)
    println(file)

// val vecA = List[Int](2,5,1)
// val vecB = List[Int](1,5,9)
// println(scalarUgly(vecA, vecB))
// println(scalar(vecA, vecB))

// println(sortUgly(List[Int](3,2,7,1,7,9,4)))
// println(sort(List[Int](3,2,7,1,7,9,4)))

// val maybePrimes = List[Int](1,3,4,6,7,8,9,10,12,13)
// maybePrimes.foreach(p => println(p, isPrimeUgly(p)))
// maybePrimes.foreach(p => println(p, isPrime(p)))

// println(primePairsUgly(10))
// println(primePairs(10))

// fileLinesUgly(filesHere.head).foreach(println(_))
// fileLines(filesHere.head).foreach(println(_))

// primePairsUgly()
// primePairs()
