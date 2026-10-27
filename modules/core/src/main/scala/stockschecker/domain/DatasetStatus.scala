package stockschecker.domain

import stockschecker.common.types.EnumType

enum DatasetStatus:
  case Available, Unavailable, ConfigurationError, Failed

object DatasetStatus extends EnumType[DatasetStatus](() => DatasetStatus.values)
