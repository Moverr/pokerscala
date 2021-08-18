package com.evolution.bootcamp.assignment.poker

import scala.collection.mutable.ListBuffer

object TexasHoldemEvaluator {


  def isFourOfAKind(board:List[String],hand:List[String]):Boolean={
    val combinedList = board ::: hand
    // find any issues combined.
    hand.foreach((item)=>{
      val count  = combinedList.filter( (x) => x(0).equals(item(0))).length;
      if(count == 4) return  true
    })

    false
  }


  def isFlush(board:List[String],hand:List[String]):Boolean={

    hand.foreach((item)=>{
      val count  = ( board ::: hand).filter( (x) => x(1).equals(item(1))).length;
      if(count == 5) return  true
    })

    false
  }

  def isStraight(board:List[String],hand:List[String]):Boolean={

    var cleenedList = ListBuffer[Int]();
    (board ::: hand).foreach(card=>{

      card(0) match {
        case 'J' => cleenedList.addOne(11)
        case 'Q' =>  cleenedList.addOne(12)
        case 'K' =>   cleenedList.addOne(13)
        case 'A' =>cleenedList += 1
        case other => {
          val cardNumber = other.toString().trim().toInt
          cleenedList.addOne(cardNumber)
        }

      }
    })

    val seqList = cleenedList.toSeq.sorted
    var inRow:Int = 0
    var prev:Int = 0
    var current:Int  = 0
    var count = 1;
    seqList.distinct.foreach((x)=>{

      current = x.toString.trim.toInt

      if(current - prev == 1 ){
        inRow = 1;
        count  = count +1;

      }else{
        if(count < 5){
          inRow = 0;
          count = 1;
        }

      }
      prev = current

    })

    if(count >= 5) true else   false
  }

  def isStraightFlush(board:List[String],hand:List[String]):Boolean={
    val  sameSuit = (board ::: hand).filter((item)=>item(1).equals(hand(0)(1))).length
    if(sameSuit >=5)  isStraight(board,hand) else false
  }



  def isRoyalFlush(board:List[String],hand:List[String]):Boolean={

    val royalFlushItems  =  List("10","J","Q","K","A")

    var sameSuitItems = ( board ::: hand).filter((item)=>item(1).equals(hand(0)(1)))
    var matchingItems = 0;
    if(sameSuitItems.length == 5){
      royalFlushItems.foreach((item)=>{
        val count  =  sameSuitItems.filter((x)=>x(0).equals(item)).length
        if(count == 1) matchingItems =  matchingItems + 1

      })
    }
    if(matchingItems == 5) true else false

  }

  def isThreeOfAKind(board:List[String],hand:List[String]):Boolean={
    hand.foreach((item)=>{
      val count  = (board ::: hand).filter( (x) => x(0).equals(item(0))).length;
      if(count == 3) return  true
    })

    false
  }


  def isTwoPair(board:List[String],hand:List[String]):Boolean={
    var numOfCardPairs  = 0
    hand.foreach((item)=>{
      val count  = (board ::: hand).filter( (x) => x(0).equals(item(0))).length;
      if(count == 2)  numOfCardPairs = numOfCardPairs +1;
    })

    if(numOfCardPairs ==  2)   true else   false
  }


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


  def convertToArray(stringChar: String): List[String] ={

    val item = stringChar.toCharArray
    var arrayItems:ListBuffer[String] = new ListBuffer[String]();
    var count = 1;
    for(record <-0 to item.length -1){
      if(count %2 == 0 ){
        var card:String = item(record-1) + ""+item(record);
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

  }
  def  getWinner(board:String,hands:List[String]): String ={

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

    val sortedlist = handsMap.toSeq.sortWith(_._1>_._1);

    sortedlist.toString()
  }



}
