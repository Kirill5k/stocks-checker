package stockschecker.domain

import stockschecker.common.types.EnumType

/** Safe operational categories; provider response text must never become a deferral reason. */
enum ProviderDeferralReason(val value: String):
  case ConcurrencyLimit extends ProviderDeferralReason("concurrency-limit")
  case RequestSpacing extends ProviderDeferralReason("request-spacing")
  case DailyQuota extends ProviderDeferralReason("daily-quota")
  case Provider429 extends ProviderDeferralReason("provider-429")

object ProviderDeferralReason extends EnumType[ProviderDeferralReason](() => ProviderDeferralReason.values, _.value)
