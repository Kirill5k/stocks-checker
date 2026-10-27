package stockschecker.domain

import stockschecker.common.types.EnumType

enum EvaluationStatus:
  case Pending, Complete, Incomplete

object EvaluationStatus extends EnumType[EvaluationStatus](() => EvaluationStatus.values)
