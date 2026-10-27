package stockschecker.domain

import stockschecker.common.types.EnumType

enum Pillar:
  case Quality, Value, Growth, Resilience

object Pillar extends EnumType[Pillar](() => Pillar.values)
