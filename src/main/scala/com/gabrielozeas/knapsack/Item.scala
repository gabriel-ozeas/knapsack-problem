package com.gabrielozeas.knapsack

import scala.annotation.tailrec
import java.io.File

case class Item(idx: Int, value: Int, weight:Int) { lazy val density = { value / weight } }

final case class Choice(capacity: Int, taken: List[Item], considerable: List[Item] ) {
	lazy val value  : Int = { taken.map {item => item.value}.sum }
	lazy val weight : Int = { taken.map {item => item.weight}.sum }
	lazy val exceed : Boolean = { weight > capacity }
	
	lazy val optimalEvaluation = { sum(taken ++ considerable, 0, capacity) }
	
	@tailrec
	def sum(xs: List[Item], accum: Int, capacity: Int): Int = {
        xs match {
	      case x :: tail => 
	        	if (accum + x.value <= capacity) sum(tail, accum + x.value, capacity) 
	        	else accum + x.value
	      case Nil => accum 
	    }
	}
}

final class DFSSolver {
  var bestChoice: Choice = null
  
  def solve(allItems: List[Item], capacity: Int): Choice = {
		val sortedItem = allItems.filter{item: Item => item.weight < capacity}.sortWith((first:Item, second: Item) => first.density > second.density)
		
		traverse(List(new Choice(capacity, List(), sortedItem)))
		return bestChoice
  }
  
  @tailrec
  def traverse(choices: List[Choice]): Unit = { 
    	choices match {
		      case actualChoice :: otherChoices => {
	    		 if (bestChoice != null && bestChoice.optimalEvaluation > actualChoice.optimalEvaluation) {
	    		   return traverse(otherChoices)
	    		 } else {
	    		   actualChoice.considerable match {
			           case Nil => 
			             	if (bestChoice != null && bestChoice.value < actualChoice.value) {
			             	  bestChoice = actualChoice
			             	  println(bestChoice.value + ", " + bestChoice.optimalEvaluation)
			             	}  else if (bestChoice == null) {
			             	  bestChoice = actualChoice
			             	}
			             	return traverse(otherChoices)
			             	
			           case firstItem :: otherItems => {
			             
			        	   val leftCapacity: Int = actualChoice.capacity - actualChoice.weight

			        	   val rightConsiderableItems: List[Item] = actualChoice.considerable.filter { item: Item => item.weight <= leftCapacity}
			               
			               rightConsiderableItems match {
			                 case x :: xs => {
			                	 val takeTheItem = new Choice(actualChoice.capacity, x :: actualChoice.taken, xs)
				        	     val dontTakeTheItem = new Choice(actualChoice.capacity, actualChoice.taken, xs)
			                	 
			                	 if (takeTheItem.exceed) {
			                	   traverse(dontTakeTheItem :: otherChoices)
			                	 } else {
			                	   if (bestChoice != null && bestChoice.optimalEvaluation > dontTakeTheItem.optimalEvaluation) {
			                		   traverse(takeTheItem :: otherChoices)  
			                	   } else {
			                		   traverse(takeTheItem :: dontTakeTheItem :: otherChoices)
			                	   }
			                	 }
			                 } 
			                 case Nil => traverse(otherChoices)
			               }
			           } 
			       }
	    		 }
		      }
		      case Nil => 
    	}
  }	
}

object Runner {
  def main (args: Array[String]) {
    
    val lines = scala.io.Source.fromFile(new File("src/main/resources/data/ks_200_0")).getLines
    val firstRow = lines.next.split(" ")
    val capacity = firstRow(1).toInt
    
    
    def extractItems(lines: Iterator[String], items: List[Item], idx: Int): List[Item] = {
      if (lines.hasNext) {
        val row = lines.next.split(" ")
        new Item(idx, row(0).toInt, row(1).toInt) :: extractItems(lines, items, idx + 1)
      } else {
        items
      }
    }
    
    val items: List[Item] = extractItems(lines, List(), 1)
    val choice: Choice = new DFSSolver().solve(items, capacity)
    
    println(choice.value)    
  }  
}
