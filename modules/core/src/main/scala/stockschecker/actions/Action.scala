package stockschecker.actions

import stockschecker.common.types.EnumType

object Action extends EnumType[Action](() => Action.values)
enum Action:
  case FetchLatestStocks