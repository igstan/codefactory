package codefactory
package turnstile

object PhantomTypes {
  sealed trait State

  object State {
    sealed trait Locked extends State
    sealed trait Unlocked extends State
  }

  trait Turnstile[S <: State] {
    def push(implicit evidence: S =:= State.Unlocked): Turnstile[State.Locked]
    def coin(implicit evidence: S =:= State.Locked): Turnstile[State.Unlocked]
  }
}
