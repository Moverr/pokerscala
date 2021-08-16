package com.evolution.bootcamp.assignment.poker

import scala.collection.mutable.ListBuffer

object TexasHoldemEvaluator {


  def isFourOfAKind(board:List[String],hand:List[String]):Boolean={
    val combinedlist = board ::: hand
    // find any issues combined.
    hand.foreach((item)=>{
      val count  = combinedlist.filter( (x) => x(0).equals(item(0))).length;
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

  
}
