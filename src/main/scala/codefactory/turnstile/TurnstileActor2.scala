package codefactory
package turnstile

import akka.actor.Actor

final class TurnstileActor2 extends Actor {
  import TurnstileActor2._

  val receive: Receive = loop(State.initial)

  def loop(state: State): Receive = {
    case Coin => context.become( loop(state.coin) )
    case Push => context.become( loop(state.push) )
  }
}

object TurnstileActor2 {
  case object Coin
  case object Push

  sealed trait State {
    def push: State
    def coin: State
  }

  object State {
    val initial = Locked(coinCount = 0, passCount = 0)
  }

  // How many coins have we accumulated.
  //private var coinCount: Int = 0
  // How many people have passed the turnstile.
  //private var passCount: Int = 0
  final case class Locked(coinCount: Int, passCount: Int) extends State {
    def push: Locked = this
    def coin: Unlocked = Unlocked(coinCount + 1, passCount)
  }

  final case class Unlocked(coinCount: Int, passCount: Int) extends State {
    def push: Locked = Locked(coinCount, passCount + 1)
    def coin: Unlocked = Unlocked(coinCount + 1, passCount)
  }
}
