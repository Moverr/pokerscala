package com.evolution.bootcamp.assignment.poker

import  TexasHoldemEvaluator._

object Solver {
  // TODO: implement correct solution logic
  def process(line: String): String = {
    val ErrorPrefix = "Error: "

    line.split("\\s+").toList match {
      case "texas-holdem" :: board :: hands   => {
        hands.sorted.mkString(" ")
         getWinner(board,hands)
      }
      case "omaha-holdem" :: board :: hands   => ErrorPrefix + "The solution doesn't support Omaha Hold'em"
      case "five-card-draw" :: hands          => ErrorPrefix + "The solution doesn't support Five Card Draw"
      case x :: _                             => ErrorPrefix + "Unrecognized game type"
      case _                                  => ErrorPrefix + "Invalid input"
    }
  }
}
