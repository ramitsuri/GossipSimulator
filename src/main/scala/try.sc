import scala.collection.mutable.ListBuffer
import scala.util.Random


var numNodes = 26

var cubeRootOfNumnodes = math.cbrt(numNodes)
if(cubeRootOfNumnodes%1!=0){
  cubeRootOfNumnodes = cubeRootOfNumnodes.toInt + 1
  numNodes = math.pow(cubeRootOfNumnodes, 3).toInt
}
println(cubeRootOfNumnodes)
val squareOfCubeRoot = math.pow(cubeRootOfNumnodes, 2)
var nodesOnTop:ListBuffer[Int] = new ListBuffer[Int]
var nodesOnBottom:ListBuffer[Int] = new ListBuffer[Int]
var nodesOnRight:ListBuffer[Int] = new ListBuffer[Int]
var nodesOnLeft:ListBuffer[Int] = new ListBuffer[Int]
var nodesAtBack:ListBuffer[Int] = new ListBuffer[Int]
var nodesAtFront:ListBuffer[Int] = new ListBuffer[Int]
nodesAtFront = ListBuffer.range(0, squareOfCubeRoot.toInt)
nodesAtBack = ListBuffer.range((numNodes-squareOfCubeRoot).toInt, numNodes)
nodesOnLeft = nodesAtFront.map(_*cubeRootOfNumnodes.toInt)
nodesOnRight = nodesOnLeft.map(_+(cubeRootOfNumnodes-1).toInt)
for(j<-0 to (cubeRootOfNumnodes-1).toInt)
  {
    nodesOnTop ++= (nodesAtFront.take(cubeRootOfNumnodes.toInt).map(_+(squareOfCubeRoot*j).toInt))
  }
nodesOnBottom ++= nodesOnTop.map(_+(squareOfCubeRoot-cubeRootOfNumnodes).toInt)
/*
nodesOnTop ++=  nodesAtFront.take(cubeRootOfNumnodes.toInt).map(_+(squareOfCubeRoot.toInt)*0)
nodesOnTop ++= nodesAtFront.take(cubeRootOfNumnodes.toInt).map(_+(squareOfCubeRoot.toInt)*1)
*/
/*
for(i<- 0 to numNodes-1)
  {
    println(i)
    if(i%cubeRootOfNumnodes == 0){
      //left
      println("added to left")
      nodesOnLeft.+=(i)
    }
    if ((i+1)%cubeRootOfNumnodes == 0){
      //right
      println("added to right")
      nodesOnRight.+=(i)
    }
    if (i>=0 && i< squareOfCubeRoot){
      //front
      println("added to front")
      nodesAtFront.+=(i)
    }
    if (i<numNodes && i>=(numNodes - squareOfCubeRoot)){
      //back
      println("added to back")
      nodesAtBack.+=(i)
    }
    /*else {
      for(j<- 0 to cubeRootOfNumnodes.toInt-1){
        if(i >= j*squareOfCubeRoot && i< j*squareOfCubeRoot + cubeRootOfNumnodes){
          //top
          println("added to left")
          nodesOnTop.+=(i)
        }
        else if(i>=((j+1)*squareOfCubeRoot - cubeRootOfNumnodes) && (i< (j+1)*squareOfCubeRoot)){
          //bottom
          println("added to left")
          nodesOnBottom.+=(i)
        }
      }
    }*/
     }
*/
println("back")
for(k<-0 to nodesAtBack.length-1){
  println(nodesAtBack(k))
}
println("front")
for(k<-0 to nodesAtFront.length-1){
  println(nodesAtFront(k))
}
println("top")
for(k<-0 to nodesOnTop.length-1){
  println(nodesOnTop(k))
}
println("bottom")
for(k<-0 to nodesOnBottom.length-1){
  println(nodesOnBottom(k))
}
println("left")
for(k<-0 to nodesOnLeft.length-1){
  println(nodesOnLeft(k))
}
println("right")
for(k<-0 to nodesOnRight.length-1){
  println(nodesOnRight(k))
}
/*
import scala.math._
import scala.util.Random
val edge: Int = ceil(sqrt(17)).toInt
println(edge)
println(pow(edge, 2))
val a = 4
val b = 91293
val c:Double = b.toDouble/a.toDouble
val numNodes = 10
while(true)
  println(Random.nextInt(numNodes-1))*/
/*
var list:List[Int] = Nil
for(i <- 0 to 10)
  list ::= i
for(i <- 0 to 10)
  println(list(i))*/
//var neighbours:ListBuffer[Int] = ListBuffer.range(0, 10) - 3
//neighbours.foreach(i => println(i))
//print(Random.nextInt(neighbours.size))
//println(neighbours(j))
//println(neighbours(neighbours.length))
//println(neighbours.size)
//println(neighbours(neighbours.length-2))
/*
for (i <- 0 to 10 - 1) {
  var neighbours:ListBuffer[Int] = new ListBuffer[Int]
  if (i == 0) {
    //when the actor is the first node
    neighbours.+=(i + 1)
  }
  else if (i == 10 - 1) {
    //when the actor is somewhere in between
    neighbours.+=(i - 1)
  }
  else {
    //when the actor is the last node
    neighbours.+=((i + 1) ,(i - 1))
  }
  for(j<- 0 to neighbours.size-1)
  {
    println("id is "+i+" "+neighbours(j))
  }
}*/