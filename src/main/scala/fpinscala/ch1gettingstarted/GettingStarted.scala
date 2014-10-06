package fpinscala.ch1gettingstarted

import scala.annotation.tailrec

object GettingStarted extends App {
	
	println("Getting Started")
	println(s"fib(10) = ${fib(10)}")
	println(s"fib(6) = ${fib(6)}")
	println(s"fib(7) = ${fib(7)}")
	println(s"fib2(7) = ${fib2(7)}")

	val s1=isSorted[Int]( Array[Int](7,8,9), (a:Int,b:Int) => a <= b )
	println(s"ordered(7,8,9) = $s1")
	val s2=isSorted( Array[Int](9,8,7), (a:Int,b:Int) => a <= b )
	println(s"ordered(9,8,7) = $s2")
	val s3=isSorted( 1.to(2000).toArray, (a:Int,b:Int) => a <= b )
	println(s"ordered(1,2,...) = $s3")


	def fib(n:Int):Int = {
		@tailrec
		def fibAcc(m:Int,prev:Int, prevprev:Int) : Int = {
			if(m==1) fibAcc(2,0,0)
			else if (m==2) fibAcc(3,1,0)
			else if(m==n) prev+prevprev
			else fibAcc(m+1,prev+prevprev,prev)			
		}
		fibAcc(1,0,0) 
	}


	def fib2(n:Int):Int = {

		@tailrec
		def fibAcc(n:Int, previous:Int, current:Int) : Int = {
			if(n==1) previous
			else fibAcc(n-1,current,previous+current)
		}
		fibAcc(n,0,1)
	}

	@tailrec
	def isSorted[A](as:Array[A], ordered : (A,A) => Boolean) : Boolean = {
		if(as.isEmpty) true
		else if(as.size == 1) true
		else if (ordered(as.head,as.tail.head)) {
			isSorted(as.tail,ordered)
		}
		else false

	}

	// Only one implementation of type sig that compiles?
	def curry[A,B,C](f:(A,B)=>C) : A => (B => C) = a => ( b => f(a,b) ) // or without using type inference (a:A) => ((b:B) => f(a,b))

	def uncurry[A,B,C](f: A => B => C) : (A,B) => C = (a,b) => f(a)(b)

	def compose[A,B,C](f: B => C, g: A => B) : A => C = a => f(g(a))

	// polymorphism limits what the function is can do with its args - unless we use escape hatches (warts) like toString, or ==
	// typically pass in polymorphic functions (like 'ordered' in isSorted) as args



}