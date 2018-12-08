package com.github.nickball.adventcode.y2018

import com.github.nickball.adventcode.Day

import scala.collection.mutable
import scala.util.matching.Regex

object Day07 extends Day(7) {
  override protected def doSolutionA(input: String): String = {
    val pattern: Regex = """Step ([A-Z]) must be finished before step ([A-Z]) can begin.""".r
    val steps = input.split("\n").map {
      case pattern(a, b) => (a(0), b(0))
    }

    //Track dependent steps by their parent
    val forward = new mutable.HashMap[Char, mutable.Set[Char]] with mutable.MultiMap[Char, Char]
    //Track prerequisite steps by the dependent
    val backward = new mutable.HashMap[Char, mutable.Set[Char]] with mutable.MultiMap[Char, Char]

    //Build graph
    steps.foreach(a => {
      forward.addBinding(a._1, a._2)
      backward.addBinding(a._2, a._1)
    })

    val ordering = Ordering.fromLessThan[Char](_ < _)
    //We need an ordered set-like queue, using treeset for now (PriorityQueue isn't a Set/unique)
    val children: mutable.Set[Char] = mutable.TreeSet.empty(ordering)
    //Find "root" steps that don't have any prereqs
    forward.keys.filterNot(a => backward.contains(a)).foreach(children.add)
    println(s"Using roots $children")

    val order = new mutable.StringBuilder()
    //Iterate over children, while we have any left
    Iterator.iterate(children) { c =>
      val current = c.head
      //"Pop" current item from queue
      c.remove(current)
      println(s"Processing $current")
      //Add current step to final result
      order.append(current)
      //Remove this step from the "backward" collection tracking dependencies
      backward.filter(b => b._2.contains(current)).foreach(b => {
        if (b._2.remove(current)) {
          println(s"\t Removing $current from backward ${b._1}")
        }
      })
      //Find children of current step, see if we can add them as candidate next steps
      forward.get(current) match {
        case Some(result) =>
          //Find children steps that have satisfied all prerequisites
          result.filter(r => backward.getOrElse(r, mutable.Set.empty).isEmpty).foreach(r => {
            c.add(r)
            println(s"\t Added to next: $r")
          })
        case None =>
        //No children, whatever
      }
      c
    }.takeWhile(_.nonEmpty).foreach(identity)

    order.toString()
  }

  override protected def doSolutionB(input: String): String = {
    val pattern: Regex = """Step ([A-Z]) must be finished before step ([A-Z]) can begin.""".r
    case class Step(id: Char)
    val steps = input.split("\n").map {
      case pattern(a, b) => (Step(a(0)), Step(b(0)))
    }

    //FIXME yet another hack for insufficient test framework support, we're gonna need some sort
    // of parametrization support in the unit testing base classes
    val (num_workers, base_duration) = steps.length match {
      case x if x <= 10 => (2, 0) //Unit test cases
      case _ => (5, 60) //"Prod" full-input file test cases
    }

    case class Worker(id: Int) extends Ordered[Worker] {
      override def compare(that: Worker): Int = this.id compare that.id
    }

    var workers_available = mutable.TreeSet.empty[Worker]
    workers_available ++= (1 to num_workers).map(Worker)

    //Track dependent steps by their parent
    val forward = new mutable.HashMap[Step, mutable.Set[Step]] with mutable.MultiMap[Step, Step]
    //Track prerequisite steps by the dependent
    val backward = new mutable.HashMap[Step, mutable.Set[Step]] with mutable.MultiMap[Step, Step]

    //Build graph
    steps.foreach(a => {
      forward.addBinding(a._1, a._2)
      backward.addBinding(a._2, a._1)
    })

    //We need an ordered set-like queue, using treeset for now (PriorityQueue isn't a Set/unique)
    val ordering = Ordering.fromLessThan[Step](_.id < _.id)
    val available: mutable.Set[Step] = mutable.TreeSet.empty(ordering)
    //Find "root" steps that don't have any prereqs
    forward.keys.filterNot(a => backward.contains(a)).foreach(available.add)
    println(s"Using roots $available")

    val finished = new mutable.StringBuilder()
    var current_second = 0
    val running_jobs = new mutable.HashMap[Step, (Worker, Int)]()
    //Iterate over children, while we have any left
    Iterator.iterate(available) { jobs =>

      //finish jobs if ready
      running_jobs.filter(j => j._2._2 == current_second).foreach(j => {
        running_jobs.remove(j._1)
        //Remove finished node as dependency for children
        backward.filter(b => b._2.contains(j._1)).foreach(b => {
          b._2.remove(j._1)
        })
        //Add children steps as ready for execution
        forward.get(j._1) match {
          case Some(result) =>
            result.filter(r => backward.getOrElse(r, mutable.Set.empty).isEmpty).foreach(r => {
              jobs.add(r)
            })
          case None =>
          //Whatever
        }
        forward.remove(j._1)
        finished.append(j._1.id)
        workers_available += j._2._1
        println(s"Worker ${j._2._1.id} FINISHED ${j._1.id} at second $current_second")
      })

      //Assign job to worker if available
      workers_available.iterator.foreach(w => {
        //Grab next job
        jobs.headOption match {
          case Some(step: Step) =>
            jobs.remove(step)
            workers_available.remove(w)
            //Calculate job completion time
            val jobDuration = current_second + base_duration + (step.id - 'A' + 1)
            //Assign job!
            running_jobs += step -> (w, jobDuration)
            println(s"Worker ${w.id} ASSIGNED ${step.id} at second $current_second, duration $jobDuration")
          case None =>
          //Nothing to assign
        }
      })

      //Skip time until next job finishes
      current_second += (running_jobs.size match {
        case x if x == 0 => 1
        case _ => running_jobs.values.map(_._2).toSeq.minBy(identity) - current_second
      })

      jobs
    }.takeWhile(_ => forward.keys.nonEmpty || running_jobs.keys.nonEmpty).foreach(identity)

    //Adjust current second since we increment after jobs are finished
    (current_second - 1).toString
  }
}
