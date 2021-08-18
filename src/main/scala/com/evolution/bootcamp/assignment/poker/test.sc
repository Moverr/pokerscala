import com.evolution.bootcamp.assignment.poker.Solver.process

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer


def isFourOfAKind(board:List[String],hand:List[String]):Boolean={
  val combinedlist = board ::: hand
  // find any issues combined.
  hand.foreach((item)=>{
    val count  = combinedlist.filter( (x) => x(0).equals(item(0))).length;
    if(count == 4) return  true
  })

  false
}


// any five cards of the same suit.
def isFlush(board:List[String],hand:List[String]):Boolean={
  val combinedlist = board ::: hand
  // find any issues combined.
  hand.foreach((item)=>{
    val count  = combinedlist.filter( (x) => x(1).equals(item(1))).length;
    if(count == 5) return  true
  })

  false
}

def isStraight(board:List[String],hand:List[String]):Boolean={

  val combinedlist = board ::: hand
  var cleenedList = ListBuffer[Int]();
  combinedlist.foreach((x)=>{

    x(0) match {
      case 'J' => cleenedList.addOne(11)
      case 'Q' =>  cleenedList.addOne(12)
      case 'K' =>   cleenedList.addOne(13)
      case 'A' =>cleenedList += 1
      case other => {
        val xt = other.toString().trim().toInt
        cleenedList.addOne(xt)
      }

    }
  })

  val seqlist = cleenedList.toSeq.sorted
  var inrow:Int = 0
  var prev:Int = 0
  var current:Int  = 0
  var count = 1;
  seqlist.distinct.foreach((x)=>{

    current = x.toString.trim.toInt

    if(current - prev == 1 ){
      inrow = 1;
      count  = count +1;

    }else{
      if(count < 5){
        inrow = 0;
        count = 1;
      }

    }
    prev = current

  })
//
//  println("FFFFFFFFFFF")
//  println(count)
//  println("FFFFFFFFFFF")

  if(count >= 5) true else   false
}

def isStraightFlush(board:List[String],hand:List[String]):Boolean={
  val combinedlist = board ::: hand
  val  samesuit = combinedlist.filter((item)=>item(1).equals(hand(0)(1))).length
  if(samesuit >=5)  isStraight(board,hand) else false
}


//todo: test three of a kind
var bd = List("3c","5y","4c","2s","7s");
var hd = List("Ah","2h");
isStraightFlush(bd,hd)




def isRoyalFlush(board:List[String],hand:List[String]):Boolean={
  val combinedlist = board ::: hand
  val royalflushitems  =  List("10","J","Q","K","A")

   var sortedItems = combinedlist.filter((item)=>item(1).equals(hand(0)(1)))

  var matchingItems = 0;
  if(sortedItems.length == 5){
    royalflushitems.foreach((item)=>{
     val count  =  sortedItems.filter((x)=>x(0).equals(item)).length
      if(count == 1) matchingItems =  matchingItems + 1

    })
  }
   if(matchingItems == 5) true else false

}

def isThreeOfAKind(board:List[String],hand:List[String]):Boolean={
  val combinedlist = board ::: hand
  // find any issues combined.
  hand.foreach((item)=>{
    val count  = combinedlist.filter( (x) => x(0).equals(item(0))).length;
    if(count == 3) return  true
  })

  false
}

/*
//todo: test three of a kind
var bd = List("5c","6d","Ac","As","Qs");
var hd = List("Ah","2h");
//isThreeOfAKind(bd,hd)
*/

def isTwoPair(board:List[String],hand:List[String]):Boolean={
  var numberOfCardpairs  = 0
  hand.foreach((item)=>{
    val count  = (board ::: hand).filter( (x) => x(0).equals(item(0))).length;
    if(count == 2)  numberOfCardpairs = numberOfCardpairs +1;
  })

  if(numberOfCardpairs ==  2)   true else   false
}

/*
//todo: test three of a kind
var bd = List("5c","4d","Ac","hs","Qs");
var hd = List("Ah","2h");
println("Is 2 pair of cards ")
isTwoPair(bd,hd)
*/


def isPair(board:List[String],hand:List[String]):Boolean={
  hand.foreach((item)=>{
    val count  = (board ::: hand).filter( (x) => x(0).equals(item(0))).length;
    if(count == 2) return  true
  })

  false
}


def isFullHouse(board:List[String],hand:List[String]):Boolean={
  isStraight(board,hand) && isPair(board,hand)
}


def convertToArray(stringca: String): List[String] ={

  var x = stringca.toCharArray
  var arrayItems:ListBuffer[String] = new ListBuffer[String]();
  var count = 1;
  for(b <-0 to x.length -1 ){
    if(count %2 == 0 ){
      var card:String = x(b-1) + ""+x(b);
      arrayItems +=card
    }
    count = count + 1;

  }
  arrayItems.toList
}



def getRanking(board:List[String],card:List[String]): Integer ={

  if(isRoyalFlush(board,card)){
  1
  }else if(isStraightFlush(board,card)){
2
  }
  else if(isFourOfAKind(board,card)){
3
  }
  else if(isFullHouse(board,card)){
4
  }
  else if(isFlush(board,card)){
5
  }
  else if(isStraight(board,card)){
6
  }

  else if(isThreeOfAKind(board,card)){
7
  }


  else if(isTwoPair(board,card)){
8
  }
  else if(isPair(board,card)){
9
  }

  else{
    10
  }
//  println(isThreeOfAKind(board,card))
//  println("GEt the ranking ")
//  1
}


def  getWinner(board:String,hands:List[String]): Unit ={

  //todo: get board array
  var boardArray:List[String] = convertToArray(board)

  //todo; loop through the hands
  var handsMap = collection.mutable.Map[Int,List[String]]()

  var counter =1;
  hands.foreach((x)=>{
    val handsArray:List[String] = convertToArray(x)


    val rank = getRanking(boardArray,handsArray)
    var xp:ListBuffer[String] = new ListBuffer[String]
    xp += x;


    if(handsMap.keySet.exists(_ == rank)){
      handsMap.put(rank,xp.toList ++ handsMap.get(rank).get)
    }else{
      handsMap.put(rank,xp.toList)
    }

    counter = counter +1;
  })


  //todo : lets work upon the

  var sortedlist = handsMap.toSeq.sortWith(_._1>_._1);

  println(sortedlist)
  println("blessed sunday ")
}





var line:String = "texas-holdem 5c6dAcAsQs Ks4c KdJs 2hAh Kh4h Kc7h 6h7d 2cJc"
val ErrorPrefix = "Error: "
line.split("\\s+").toList match {
  case "texas-holdem" :: board :: hands   => {
    getWinner(board,hands)
  }
  case "omaha-holdem" :: board :: hands   => ErrorPrefix + "The solution doesn't support Omaha Hold'em"
  case "five-card-draw" :: hands          => ErrorPrefix + "The solution doesn't support Five Card Draw"
  case x :: _                             => ErrorPrefix + "Unrecognized game type"
  case _                                  => ErrorPrefix + "Invalid input"
}


var x = "5c6dAcAsQs".toCharArray

def tieBreaker(): Unit ={
  val x:List[(Int,List[String])]  = List(
    (10,List("2cJc", "Kc7h", "Kh4h", "KdJs", "Ks4c")),
    (9,List("6h7d")),
    (7,List("2hAh"))
  )

  for(xa <-x){
    xa._1 match {
      case 10 =>{
        println("Work upon sorting out high cards")
//        tieBreakerhighCards(xa._2)
      }
      case other => println(other)
    }
    println(xa._1)
  }

println(x)

}
//tieBreaker()
def tieBreakerhighCards(): Unit ={
  var cardsRanked = List("A", "K", "Q", "J", 10, 9, 8, 7, 6, 5, 4, 3 ,2);
  println(cardsRanked)
  //def tieBreakerhighCards(cards:List[String]): Unit ={
  val testdata:List[String] =  List("2cJc", "Kc7h", "Kh4h", "KdJs", "Ks4c")

  println(testdata);
}

def ping(list: List[String]): Unit ={
  println(list)
}
//@tailrec
def getRanke(
           //   cardsRanked:List[Unit]
             // = List("A", "K", "Q", "J", 10, 9, 8, 7, 6, 5, 4, 3 ,2)

            ): Unit ={

  val testdata:List[String] =  List("2cJc", "Kc7h", "Kh4h", "KdJs", "Ks4c")
  var items:ListBuffer[List[String]] = new ListBuffer[List[String]]()
  testdata.foreach((x)=> {
    items += convertToArray(x)

  });
  println(items)
  items.foreach((x)=>{
    ping(x)
  })

  println("..............................")
 println(items(0) )

  val rankeditems :mutable.HashMap[String,List[List[String]]] = new mutable.HashMap[String,List[List[String]]]()

  items.foreach(x=>{
    var xp:ListBuffer[List[String]] = new ListBuffer[List[String]]
    x match {
      case ::(head, next) =>{

        if(head.trim.contains("A") || next(0).trim.contains("A") ){

          xp += x;
         if(rankeditems.keySet.exists(_ == "A")){
           rankeditems.put("A",xp.toList ++ rankeditems.get("A").get)
         }else{
           rankeditems.put("A",xp.toList)
          }


        }

        else if(head.trim.contains("K") || next(0).trim.contains("K") ){
          xp += x;
          if(rankeditems.keySet.exists(_ == "K")){
            rankeditems.put("K",xp.toList ++ rankeditems.get("K").get)
          }else{
            rankeditems.put("K",xp.toList)
          }
        }

        else if(head.trim.contains("Q") || next(0).trim.contains("Q") ){
          xp += x;
          if(rankeditems.keySet.exists(_ == "Q")){
            rankeditems.put("Q",xp.toList ++ rankeditems.get("Q").get)
          }else{
            rankeditems.put("Q",xp.toList)
          }
        }

        else if(head.trim.contains("J") || next(0).trim.contains("J") ){
          xp += x;
          if(rankeditems.keySet.exists(_ == "J")){
            rankeditems.put("J",xp.toList ++ rankeditems.get("J").get)
          }else{
            rankeditems.put("J",xp.toList)
          }
        }




        else if(head.trim.contains(10) || next(0).trim.contains(10) ){
          xp += x;
          if(rankeditems.keySet.exists(_ == "10")){
            rankeditems.put("10",xp.toList ++ rankeditems.get("10").get)
          }else{
            rankeditems.put("10",xp.toList)
          }
        }


        else if(head.trim.contains(9) || next(0).trim.contains(9) ){
          xp += x;
          if(rankeditems.keySet.exists(_ == "9")){
            rankeditems.put("9",xp.toList ++ rankeditems.get("9").get)
          }else{
            rankeditems.put("9",xp.toList)
          }
        }


        else if(head.trim.contains(8) || next(0).trim.contains(8) ){
          xp += x;
          if(rankeditems.keySet.exists(_ == "8")){
            rankeditems.put("8",xp.toList ++ rankeditems.get("8").get)
          }else{
            rankeditems.put("8",xp.toList)
          }
        }

        else if(head.trim.contains(7) || next(0).trim.contains(7) ){
          xp += x;
          if(rankeditems.keySet.exists(_ == "7")){
            rankeditems.put("7",xp.toList ++ rankeditems.get("7").get)
          }else{
            rankeditems.put("7",xp.toList)
          }
        }

        else if(head.trim.contains(6) || next(0).trim.contains(6) ){
          xp += x;
          if(rankeditems.keySet.exists(_ == "6")){
            rankeditems.put("6",xp.toList ++ rankeditems.get("6").get)
          }else{
            rankeditems.put("6",xp.toList)
          }
        }


        else if(head.trim.contains(5) || next(0).trim.contains(5) ){
          xp += x;
          if(rankeditems.keySet.exists(_ == "5")){
            rankeditems.put("5",xp.toList ++ rankeditems.get("5").get)
          }else{
            rankeditems.put("5",xp.toList)
          }
        }

        else if(head.trim.contains(4) || next(0).trim.contains(4) ){
          xp += x;
          if(rankeditems.keySet.exists(_ == "4")){
            rankeditems.put("4",xp.toList ++ rankeditems.get("4").get)
          }else{
            rankeditems.put("4",xp.toList)
          }
        }

        else if(head.trim.contains(3) || next(0).trim.contains(3) ){
          xp += x;
          if(rankeditems.keySet.exists(_ == "3")){
            rankeditems.put("3",xp.toList ++ rankeditems.get("3").get)
          }else{
            rankeditems.put("3",xp.toList)
          }
        }


        else if(head.trim.contains(2) || next(0).trim.contains(2) ){
          xp += x;
          if(rankeditems.keySet.exists(_ == "2")){
            rankeditems.put("2",xp.toList ++ rankeditems.get("2").get)
          }else{
            rankeditems.put("2",xp.toList)
          }
        }



      }
      case Nil =>


    }
   // println(x.tail);
  })


  println(".,.,.,.,.,.,.,")
  println(rankeditems);


  val cats:ListBuffer[String] = new ListBuffer[String]();
  if(items(0)(0).contains("J") || items(0)(1).contains("J")  ){
    println(". QUESTITON ABOUT ME ...")
  }
//  List("A", "K", "Q", "J", 10, 9, 8, 7, 6, 5, 4, 3 ,2)
  //check the highest card


 // if(x == 1) x else getRanke(x-1)

}
getRanke()
//tieBreakerhighCards()