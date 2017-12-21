package codefactory
package stacks

import scala.collection.mutable
import org.scalatest.{ FunSuite, Matchers }

final class StateMonadDerivationSuite extends FunSuite with Matchers {
  test("step 0: mutable stack") {
    val stack = mutable.Stack[Int]()
    stack.push(4)
    stack.push(5)
    val a = stack.pop()
    val b = stack.pop()
    val r = a + b
    assert(r == 9)
  }

  test("step 0.5: immutable stack") {
    def push[E](e: E, stack: List[E]): (Unit, List[E]) =
      ((), e :: stack)

    def pop[E](stack: List[E]): (E, List[E]) =
      stack match {
        case Nil => throw new RuntimeException("empty stack")
        case head :: tail => (head, tail)
      }

    val stack0 = List.empty[Int]
    val ((), stack1) = push(4, stack0)
    val ((), stack2) = push(5, stack1)
    val (a, stack3) = pop(stack2)
    val (b, stack4) = pop(stack3)
    val r = a + b

    assert(r == 9)
  }

  test("step 1: transform to continuation-passing style") {
    def push[E](e: E, stack: List[E]): (Unit, List[E]) =
      ((), e :: stack)

    def pop[E](stack: List[E]): (E, List[E]) =
      stack match {
        case Nil => throw new RuntimeException("empty stack")
        case head :: tail => (head, tail)
      }

    def next[A, E, R](
      result: (A, List[E])
    )(
      action: ((A, List[E])) => R
    ): R = {
      action(result)
    }

    val stack0 = List.empty[Int]
    val r =
      next(push(4, stack0)) { case ((), stack1) =>
        next(push(5, stack1)) { case ((), stack2) =>
          next(pop(stack2)) { case (a, stack3) =>
            next(pop(stack3)) { case (b, stack4) =>
              a + b
            }
          }
        }
      }

    assert(r == 9)
  }

  test("step 2: function currying + partial application on stack actions") {
    def push[E](e: E): List[E] => (Unit, List[E]) =
      stack => ((), e :: stack)

    def pop[E](): List[E] => (E, List[E]) = {
      case Nil => throw new RuntimeException("empty stack")
      case head :: tail => (head, tail)
    }

    def next[A, E, R](
      result: (A, List[E])
    )(
      action: ((A, List[E])) => R
    ): R = {
      action(result)
    }

    val stack0 = List.empty[Int]
    val r =
      next(push(4)(stack0)) { case ((), stack1) =>
        next(push(5)(stack1)) { case ((), stack2) =>
          next(pop()(stack2)) { case (a, stack3) =>
            next(pop()(stack3)) { case (b, stack4) =>
              a + b
            }
          }
        }
      }

    assert(r == 9)
  }

  test("step 3: currying and partial application on `next`") {
    def push[E](e: E): List[E] => (Unit, List[E]) =
      stack => ((), e :: stack)

    def pop[E]: List[E] => (E, List[E]) = {
      case Nil => throw new RuntimeException("empty stack")
      case head :: tail => (head, tail)
    }

    def next[A, E, R](
      action1: List[E] => (A, List[E])
    )(
      action2: ((A, List[E])) => R
    ): List[E] => R = {
      stack => action2(action1(stack))
    }

    val stack0 = List.empty[Int]
    val r =
      next(push(4)) { case ((), stack1) =>
        next(push(5)) { case ((), stack2) =>
          next(pop[Int]) { case (a, stack3) =>
            next(pop[Int]) { case (b, stack4) =>
              a + b
            }(stack3)
          }(stack2)
        }(stack1)
      }(stack0)

    assert(r == 9)
  }

  test("step 4: hide intermediate stacks inside next") {
    def push[E](e: E): List[E] => (Unit, List[E]) =
      stack => ((), e :: stack)

    def pop[E]: List[E] => (E, List[E]) = {
      case Nil => throw new RuntimeException("empty stack")
      case head :: tail => (head, tail)
    }

    def next[A, E, R](
      action1: List[E] => (A, List[E])
    )(
      callback: ((A, List[E])) => List[E] => (R, List[E])
    ): List[E] => (R, List[E]) = {
      stack =>
        val (a, newStack) = action1(stack)
        callback((a, newStack))(newStack)
    }

    val stackComputation =
      next(push(4)) { case ((), stack1) =>
        next(push(5)) { case ((), stack2) =>
          next(pop[Int]) { case (a, stack3) =>
            next(pop[Int]) { case (b, stack4) =>
              stack => (a + b, stack)
            }
          }
        }
      }

    val stack0 = List.empty[Int]
    val (result, finalStack) = stackComputation(stack0)

    assert(result == 9)
  }

  test("step 5: hide even more intermediate stacks") {
    def push[E](e: E): List[E] => (Unit, List[E]) =
      stack => ((), e :: stack)

    def pop[E]: List[E] => (E, List[E]) = {
      case Nil => throw new RuntimeException("empty stack")
      case head :: tail => (head, tail)
    }

    def next[A, E, R](
      action1: List[E] => (A, List[E])
    )(
      callback: A => List[E] => (R, List[E])
    ): List[E] => (R, List[E]) = {
      stack =>
        val (a, newStack) = action1(stack)
        callback(a)(newStack)
    }

    val stackComputation =
      next(push(4)) { _ =>
        next(push(5)) { _ =>
          next(pop[Int]) { a =>
            next(pop[Int]) { b =>
              stack => (a + b, stack)
            }
          }
        }
      }

    val stack0 = List.empty[Int]
    val (result, finalStack) = stackComputation(stack0)

    assert(result == 9)
  }

  test("step 6: introduce wrap method") {
    def push[E](e: E): List[E] => (Unit, List[E]) =
      stack => ((), e :: stack)

    def pop[E]: List[E] => (E, List[E]) = {
      case Nil => throw new RuntimeException("empty stack")
      case head :: tail => (head, tail)
    }

    def next[A, E, R](
      action1: List[E] => (A, List[E])
    )(
      callback: A => List[E] => (R, List[E])
    ): List[E] => (R, List[E]) = {
      stack =>
        val (a, newStack) = action1(stack)
        callback(a)(newStack)
    }

    def wrap[A, E](e: A): List[E] => (A, List[E]) =
      stack => (e, stack)

    val stackComputation =
      next(push(4)) { _ =>
        next(push(5)) { _ =>
          next(pop[Int]) { a =>
            next(pop[Int]) { b =>
              wrap(a + b)
            }
          }
        }
      }

    val stack0 = List.empty[Int]
    val (result, finalStack) = stackComputation(stack0)

    assert(result == 9)
  }

  test("step 7: introduce type alias for stack actions") {
    type StackAction[A, E] = List[E] => (A, List[E])

    def push[E](e: E): StackAction[Unit, E] =
      stack => ((), e :: stack)

    def pop[E]: StackAction[E, E] = {
      case Nil => throw new RuntimeException("empty stack")
      case head :: tail => (head, tail)
    }

    def next[A, B, E](action1: StackAction[A, E])(callback: A => StackAction[B, E]): StackAction[B, E] = {
      stack =>
        val (a, newStack) = action1(stack)
        callback(a)(newStack)
    }

    def wrap[A, E](e: A): StackAction[A, E] =
      stack => (e, stack)

    val stackComputation =
      next(push(4)) { _ =>
        next(push(5)) { _ =>
          next(pop[Int]) { a =>
            next(pop[Int]) { b =>
              wrap(a + b)
            }
          }
        }
      }

    val stack0 = List.empty[Int]
    val (result, finalStack) = stackComputation(stack0)

    assert(result == 9)
  }

  test("step 8: encapsulate next inside StackAction class") {
    final class StackAction[A, E](val run: List[E] => (A, List[E])) {
      def next[B](callback: A => StackAction[B, E]): StackAction[B, E] = {
        new StackAction(stack => {
          val (a, newStack) = run(stack)
          val nextAction = callback(a)
          nextAction.run(newStack)
        })
      }
    }

    def push[E](e: E): StackAction[Unit, E] =
      new StackAction(stack => ((), e :: stack))

    def pop[E]: StackAction[E, E] = new StackAction({
      case Nil => throw new RuntimeException("empty stack")
      case head :: tail => (head, tail)
    })

    def wrap[A, E](e: A): StackAction[A, E] =
      new StackAction(stack => (e, stack))

    val stackComputation =
      push(4).next { _ =>
        push(5).next { _ =>
          pop.next { a =>
            pop.next { b =>
              wrap(a + b)
            }
          }
        }
      }

    val stack0 = List.empty[Int]
    val (result, finalStack) = stackComputation.run(stack0)

    assert(result == 9)
  }

  test("step 9: introduce map + rewrite as for-comprehension") {
    final class StackAction[A, E](val run: List[E] => (A, List[E])) {
      def map[B](fn: A => B): StackAction[B, E] =
        new StackAction(stack => {
          val (a, newStack) = run(stack)
          val b = fn(a)
          (b, newStack)
        })

      def flatMap[B](callback: A => StackAction[B, E]): StackAction[B, E] = {
        new StackAction(stack => {
          val (a, newStack) = run(stack)
          val nextAction = callback(a)
          nextAction.run(newStack)
        })
      }
    }

    def push[E](e: E): StackAction[Unit, E] =
      new StackAction(stack => ((), e :: stack))

    def pop[E]: StackAction[E, E] = new StackAction({
      case Nil => throw new RuntimeException("empty stack")
      case head :: tail => (head, tail)
    })

    def wrap[A, E](e: A): StackAction[A, E] =
      new StackAction(stack => (e, stack))

    val stackComputation1: StackAction[Int, Int] =
      for {
        _ <- push(4)
        _ <- push(5)
        a <- pop
        b <- pop
      } yield {
        a + b
      }

    val stackComputation2: StackAction[Int, Int] =
      for {
        _ <- push(6)
        _ <- push(3)
        a <- pop
        b <- pop
      } yield {
        a + b
      }

    val composed = for {
      a <- stackComputation1
      b <- stackComputation2
    } yield {
      a + b
    }

    val stack0 = List.empty[Int]
    val (result, finalStack) = composed.run(stack0)

    assert(result == 18)
  }

  test("step 10: generalize to State monad") {
    final class State[A, S](val run: S => (A, S)) {
      def map[B](fn: A => B): State[B, S] =
        new State(stack => {
          val (a, newStack) = run(stack)
          val b = fn(a)
          (b, newStack)
        })

      def flatMap[B](callback: A => State[B, S]): State[B, S] = {
        new State(stack => {
          val (a, newStack) = run(stack)
          val nextAction = callback(a)
          nextAction.run(newStack)
        })
      }

      def runA(state: S): A =
        run(state)._1

      def runS(state: S): S =
        run(state)._2
    }

    object State {
      def pure[A, E](e: A): State[A, E] =
        new State(stack => (e, stack))
    }

    object Stack {
      def push[E](e: E): State[Unit, List[E]] =
        new State(stack => ((), e :: stack))

      def pop[E]: State[E, List[E]] = new State({
        case Nil => throw new RuntimeException("empty stack")
        case head :: tail => (head, tail)
      })
    }

    val stackComputation1: State[Int, List[Int]] =
      for {
        _ <- Stack.push(4)
        _ <- Stack.push(5)
        a <- Stack.pop
        b <- Stack.pop
      } yield {
        a + b
      }

    val stackComputation2 =
      for {
        _ <- Stack.push(6)
        _ <- Stack.push(3)
        a <- Stack.pop
        b <- Stack.pop
      } yield {
        a + b
      }

    val composed = for {
      a <- stackComputation1
      b <- stackComputation2
    } yield {
      a + b
    }

    val stack0 = List.empty[Int]
    val result = composed.runA(stack0)

    assert(result == 18)
  }
}
