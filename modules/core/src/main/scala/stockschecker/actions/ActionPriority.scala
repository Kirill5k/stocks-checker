package stockschecker.actions

object ActionPriority:
  val Internal   = -1
  val Normal     = 0
  val Evaluation = 1

  def forAction(action: Action): Int = action match
    case _: Action.RefreshEvaluationPrices => Evaluation
    case action if !action.isSchedulable   => Internal
    case _                                 => Normal
