
package gossip;

import scala.math._
import scala.util.Random
import akka.actor.Actor
import akka.actor.Props
import akka.actor._
import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props

case class Quit()
case class Gossip()
case class PushSumGossip(sum:Double, weight:Double)
case class Test()
case class AddPlayer(addPlayer:Int)
case class RemovePlayer(removePlayer:Int)
case class InitPlayer(uniqueId:Int, allPlayers:List[ActorRef], neighbor:List[Int], statusKeeper:ActorRef, rumorBreak:Int, divergence:Int)
case class ActorStatus(playerId:Int, gossipCount:Int)
case class PushSumStatus(playerId:Int)
case class InitKeeper(system:ActorSystem, nodeCount:Int, rumorBreak:Int, algorithm:String)

object Gossip {
  def main(args: Array[String]): Unit = {
    if(args.length == 0 || args.length != 3){
      /*println("Arguments are not proper.")*/
      /*args(0) = "10"
      args(1) = "full"
      args(2) = "gossip"*/
    /*}else if(args.length == 3){*/
      val full:String = "full";
      val twoD:String = "2D";
      val line:String = "line";
      val impTwoD:String = "imp2D";

      val gossip:String = "gossip";
      val pushSum:String = "push-sum";

      val rumorBreak:Int = 10;
      val divergence:Int = 2;

     /* var numNodes:Int = args(0).toInt
      var topology:String = args(1)
      var algorithm:String = args(2)*/
     var numNodes:Int = 10
      var topology:String = line
      var algorithm:String = pushSum

      if(topology==twoD || topology==impTwoD) numNodes = getNextPerfectSquare(numNodes)
      val numNodesRoot = math.sqrt(numNodes.toDouble);

      val system = ActorSystem("GroupCommunicationSystem")

      var statusKeeper:ActorRef = system.actorOf(Props[StatusKeeper])
      var allPlayers:List[ActorRef] = Nil
      var i:Int = 0
      while(i<numNodes){
        allPlayers ::= system.actorOf(Props[Player])
        i += 1
      }

      if(full.equalsIgnoreCase(topology)){
        //println("Inside Full ...")
        var i:Int = 0
        while(i<allPlayers.length){
          var neighbors:List[Int] = Nil

          var j:Int = 0
          while(j<allPlayers.length){
            if(j!=i){
              neighbors ::= j
            }
            j += 1
          }
          allPlayers(i) ! InitPlayer(i, allPlayers, neighbors, statusKeeper, rumorBreak, divergence)
          i += 1
        }
        //println("Completed Full ...")
      }

      if(twoD.equalsIgnoreCase(topology)){
        var i:Int = 0
        while(i<allPlayers.length){
          var neighbors:List[Int] = Nil

          if(!isNodeOnBottom(i, allPlayers.length)) neighbors ::= (i+numNodesRoot.toInt)
          if(!isNodeOnTop(i, allPlayers.length)) neighbors ::= (i-numNodesRoot.toInt)
          if(!isNodeOnLeft(i, allPlayers.length)) neighbors ::= (i-1)
          if(!isNodeOnRight(i, allPlayers.length)) neighbors ::= (i+1)

          allPlayers(i) ! InitPlayer(i, allPlayers, neighbors, statusKeeper, rumorBreak, divergence)

          i += 1
        }
      }
      if(line.equalsIgnoreCase(topology)){
        var i:Int = 0;
        while(i<allPlayers.length){
          var neighbors:List[Int] = Nil

          if(i>0) neighbors ::= (i-1)
          if(i<allPlayers.length-1) neighbors ::= (i+1)

          allPlayers(i) ! InitPlayer(i, allPlayers, neighbors, statusKeeper, rumorBreak, divergence)

          i += 1
        }
      }

      if(impTwoD.equalsIgnoreCase(topology)){
        var i:Int = 0
        while(i<allPlayers.length){
          var neighbors:List[Int] = Nil
          var tempList:List[Int] = Nil

          if(!isNodeOnBottom(i, allPlayers.length)) { neighbors ::= (i+numNodesRoot.toInt); tempList ::= (i+numNodesRoot.toInt); }
          if(!isNodeOnTop(i, allPlayers.length)) { neighbors ::= (i-numNodesRoot.toInt); tempList ::= (i-numNodesRoot.toInt); }
          if(!isNodeOnLeft(i, allPlayers.length)) { neighbors ::= (i-1); tempList ::= (i-1); }
          if(!isNodeOnRight(i, allPlayers.length)) { neighbors ::= (i+1); tempList ::= (i+1); }

          var randomInt:Int = -1;
          do{
            randomInt = Random.nextInt(allPlayers.length)
            for(x<-tempList){
              if(randomInt == x) randomInt = -1
            }
          }while(randomInt == -1)

          neighbors ::= (randomInt)
          allPlayers(i) ! InitPlayer(i, allPlayers, neighbors, statusKeeper, rumorBreak, divergence)

          i += 1
        }
      }

      statusKeeper ! InitKeeper(system, numNodes, rumorBreak, algorithm)
      if(algorithm.equalsIgnoreCase(gossip)){
        allPlayers(0) ! Gossip
      }else{
        var sequences = 10
        if(numNodes<sequences) sequences = numNodes

        for(c <- 0 until sequences){
          //println("c: "+c)
          var randomPlayer = Random.nextInt(allPlayers.length)
          allPlayers(randomPlayer) ! PushSumGossip(0,1)
        }

      }


    }
    def getNextPerfectSquare (num:Int):Int = {
      var bi:Int = num
      while(math.sqrt(bi.toDouble)%1!=0){
        bi+=1
      }
      return bi
    }

    def isNodeOnTop (nodeNum:Int, nodesLength:Int):Boolean = {
      if(nodeNum.toDouble<math.sqrt(nodesLength.toDouble)) return true
      return false;
    }
    def isNodeOnBottom (nodeNum:Int, nodesLength:Int):Boolean = {
      if(nodeNum.toDouble>=(nodesLength.toDouble - math.sqrt(nodesLength.toDouble))) return true
      return false;
    }
    def isNodeOnLeft (nodeNum:Int, nodesLength:Int):Boolean = {
      if(nodeNum.toDouble % math.sqrt(nodesLength.toDouble)==0) return true
      return false;
    }
    def isNodeOnRight (nodeNum:Int, nodesLength:Int):Boolean = {
      if((nodeNum.toDouble+1) % math.sqrt(nodesLength.toDouble)==0) return true
      return false;
    }
  }
}

case class Player() extends Actor{
  var rumorTerminationGossip = 0
  var rumorTerminationPush = math.pow(10, -10)
  var playerId:Int = 0
  var allPlayers:List[ActorRef] = Nil

  var statusKeeper:ActorRef = null
  var neighbors:List[Int] = Nil
  var rumorCounter:Int = 0
  var stabilityCounter:Int = 0;
  var div = 0

  var s:Double = 0
  var w:Double = 0

  def receive = {
    case InitPlayer(uniqueId:Int, allNodes:List[ActorRef], neighborList:List[Int], statActor:ActorRef, rumorBreak:Int, divergence:Int) => {
      neighbors = neighbors ::: neighborList
      playerId = uniqueId
      statusKeeper = statActor
      rumorTerminationGossip = rumorBreak
      s = uniqueId
      //w = 16-uniqueId
      div = divergence
      allPlayers = allNodes

    }


    case Gossip => {
      if(rumorCounter<rumorTerminationGossip) {
        //println("Received ping to ID: "+playerId+" Counter -- "+rumorCounter)
        rumorCounter += 1;
        statusKeeper ! ActorStatus(playerId, rumorCounter)

        var randomPlayer = 0

        for(c<- 0 until div){
          randomPlayer = Random.nextInt(neighbors.length)
          //println("From player: "+playerId+" Sending message to "+randomPlayer)
          allPlayers(neighbors(randomPlayer)) ! Gossip
        }
      }
    }

    case PushSumGossip(sum:Double, weight:Double) => {
      //println("PlayerID: "+playerId+"  Receives S/W: "+(sum/weight)+" -- Self s/w: "+(s/w));
      rumorCounter += 1;
      div = 2
      var oldSbyW:Double = s/w;
      s+=sum;
      w+=weight;
      s = s/div;
      w = w/div;
      var newSbyW:Double = s/w;

      //println("Inside PushSum ---- oldSbyW: "+oldSbyW+"   --- newSbyW: "+newSbyW);

      if(rumorCounter==1 || Math.abs((oldSbyW-newSbyW))>rumorTerminationPush) {
        stabilityCounter=0;

        for(c<- 1 until div){
          //println("From Actor: "+playerId+"\tSending Message ... Sum: "+s+" Weight: "+w);
          var randomPlayer = Random.nextInt(neighbors.length);
          allPlayers(neighbors(randomPlayer)) ! PushSumGossip(s,w)
        }

      }else{
        stabilityCounter+=1;
        if(stabilityCounter>3) {
          //println("Final Condition --- For Actor  "+playerId+" \tRumor Count  "+rumorCounter+" \ts/w: "+(s/w));

          statusKeeper ! PushSumStatus(playerId);
          self ! PoisonPill
        }else{
          for(c<- 1 until div){
            //println("From Actor: "+playerId+" Sending Message ... Sum: "+s+" Weight: "+w);
            var randomPlayer = Random.nextInt(neighbors.length);
            allPlayers(neighbors(randomPlayer)) ! PushSumGossip(s,w)
          }
        }
      }
    }
  }
  override def postStop(){
    //println("Post Terimation --- For Actor  "+playerId+" \tRumor Count  "+rumorCounter+" \ts/w: "+(s/w));
  }
}



class StatusKeeper extends Actor{
  var b:Long = 0;
  var numNodes:Int = 0;
  var rumorBreak:Int = 0;
  var keeperSys:ActorSystem = null;
  var shouldWork:Boolean = false;
  var stableNodeCount:Int = 0;

  //println("Status Keeper Started ...")
  b = System.currentTimeMillis
  //println("Start Time: "+b)
  var statusList:List[ActorStatus] = Nil
  def receive = {
    case InitKeeper(sys:ActorSystem, nodeCount:Int, rumorBreakLimit:Int, algorithm:String) => {
      b = System.currentTimeMillis
      keeperSys = sys;
      numNodes = nodeCount;
      rumorBreak = rumorBreakLimit;
      if(algorithm.equalsIgnoreCase("gossip")) shouldWork = true;
    }

    case ActorStatus(id:Int, count:Int) => {
      var tempStatusList:List[ActorStatus] = Nil
      var i:Int = 0
      while(i<statusList.length){
        //if(statusList(i).playerId==id) statusList.drop(i);
        if(statusList(i).playerId!=id) tempStatusList ::= statusList(i)
        i += 1
      }
      statusList = tempStatusList
      statusList = statusList ::: List(new ActorStatus(id, count))

      //println("Status received from Player"+id+"  count: "+count+ "  StatusListCount : "+statusList.length)

      if((statusList.length.toDouble/numNodes.toDouble)>0.90){
        keeperSys.shutdown;
      }
    }

    case PushSumStatus(playerId:Int) => {
      //println("Stability acheived by :"+playerId+"  stableNodeCount: "+stableNodeCount+"  numNodes: "+numNodes);
      keeperSys.shutdown;
    }
  }

  override def postStop(){
    //println("Post Stop Test Text ...")
    printStatus(statusList)
  }
  def printStatus(statusList:List[ActorStatus]){
    if(shouldWork){
      for(r <-0 until rumorBreak+1){
        var count:Int = 0;
        for(status<-statusList){
          if(status.gossipCount==r) count += 1;
        }
        if(r==0) count = (numNodes - statusList.length)
        //println("Number of Actor who received Gossip "+r+" times : "+count);
      }
      //println("Total Coverage: "+statusList.length+" / "+numNodes+" => "+100*(statusList.length.toDouble/numNodes.toDouble)+"%")
    }
    println(b-System.currentTimeMillis)
  }
}