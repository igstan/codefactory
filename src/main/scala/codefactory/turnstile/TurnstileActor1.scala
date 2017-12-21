package codefactory
package turnstile

import akka.actor.Actor

final class TurnstileActor1 extends Actor {
  import TurnstileActor1._

  // How many coins have we accumulated.
  private var coinCount: Int = 0

  // How many people have passed the turnstile.
  private var passCount: Int = 0

  def receive: Receive = locked

  def locked: Receive = {
    case Coin =>
      coinCount += 1
      context.become(unlocked)
    case Push =>
      () // emit warning, maybe
  }

  def unlocked: Receive = {
    case Coin =>
      coinCount += 1
    case Push =>
      passCount += 1
      context.become(locked)
  }
}

object TurnstileActor1 {
  case object Coin
  case object Push
}
