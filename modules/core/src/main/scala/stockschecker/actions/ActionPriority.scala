package stockschecker.actions

object ActionPriority:
  val Normal     = 0
  val Evaluation = 1

  def forAction(action: Action): Int = action match
    case _: Action.RefreshEvaluationPrices => Evaluation
    case _                                 => Normal
