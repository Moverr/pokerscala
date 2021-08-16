import com.evolution.bootcamp.assignment.poker.Solver.process
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





def  getWinner(board:String,hands:List[String]): Unit ={

  //todo: get board array
  var boardArray:List[String] = convertToArray(board)

  //todo; loop through the hands
  var handsMap = collection.mutable.Map[Int,List[String]]()

  var counter =1;
  hands.foreach((x)=>{
    var handsArray:List[String] = convertToArray(x)
    handsMap += (counter->handsArray);
    println(counter);
    counter = counter +1;
  })


  //todo: get best matching .. of this game


  println(handsMap)
  //println(gethandArray(board).mkString("::"))
  //println("sssss")
  //todo: players = hands.length
  //todo: get existance of a pair of hand.
  //todo: sutits have  ann issue to play
  //todo: majorly characters have an issue to play/
}


var line:String = "texas-holdem 5c6dAcAsQs Ks4c KdJs 2hAh Kh4h Kc7h 6h7d 2cJc"
val ErrorPrefix = "Error: "
line.split("\\s+").toList match {
  case "texas-holdem" :: board :: hands   => {
    println(board)
    println(hands)
    getWinner(board,hands)
  }
  case "omaha-holdem" :: board :: hands   => ErrorPrefix + "The solution doesn't support Omaha Hold'em"
  case "five-card-draw" :: hands          => ErrorPrefix + "The solution doesn't support Five Card Draw"
  case x :: _                             => ErrorPrefix + "Unrecognized game type"
  case _                                  => ErrorPrefix + "Invalid input"
}


var x = "5c6dAcAsQs".toCharArray


