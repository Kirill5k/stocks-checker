package stockchecker

import stockchecker.common.types.StringType

package object domain {
  opaque type Ticker = String
  object Ticker extends StringType[Ticker]
}
