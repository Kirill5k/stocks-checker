package stockschecker.domain.investment

import stockschecker.domain.*

import java.time.{Instant, LocalDate, YearMonth, ZoneOffset}

object EvaluationCapture:
  final case class Context(screen: InvestmentScreen, monthly: Option[InvestmentEvaluation], finished: Option[InvestmentEvaluation]):
    def settledReport(now: Instant): Option[InvestmentEvaluation] =
      val finalThrough = InvestmentEvaluation.finalCaptureThrough(formed(screen))
      monthly
        .orElse(Option.when(finalThrough.isBefore(LocalDate.ofInstant(now, ZoneOffset.UTC)))(finished).flatten)
        .filter(_.settled)

  final case class State(requestedHistory: Set[Ticker])
  final case class Task(screen: InvestmentScreen, month: YearMonth, existing: Option[State])
  final case class Draft(report: InvestmentEvaluation, through: LocalDate, needed: List[Ticker]):
    def withPending(jobs: Set[JobId]): InvestmentEvaluation = report.copy(pendingDataJobs = jobs.toList.sortBy(_.value))

    def settle(jobs: Set[JobId]): InvestmentEvaluation =
      val unresolved = Option.when(jobs.isEmpty)(needed.map(_.value)).getOrElse(Nil)
      val issues     = Option
        .when(unresolved.nonEmpty)(s"Price history remains unavailable after completed capture jobs for: ${unresolved.mkString(", ")}")
        .toList
      withPending(jobs).copy(
        settled = jobs.isEmpty,
        horizons = report.horizons.map(h => h.copy(issues = h.issues ++ issues))
      )

  private def formed(screen: InvestmentScreen): LocalDate = LocalDate.ofInstant(screen.createdAt, ZoneOffset.UTC)

  def range(screen: InvestmentScreen, month: YearMonth): DateRange =
    val from = formed(screen)
    DateRange(from, InvestmentEvaluation.captureThrough(from, month))

  def hasCapture(screen: InvestmentScreen, month: YearMonth): Boolean =
    val from = formed(screen)
    InvestmentEvaluation.captureThrough(from, month).isAfter(from)

  def tickers(screen: InvestmentScreen): List[Ticker] =
    (InvestmentEvaluator.Benchmark :: screen.cohorts.flatMap(_.tickers)).distinct

  def pending(screen: InvestmentScreen, now: Instant, month: YearMonth): InvestmentEvaluation =
    InvestmentEvaluator.evaluate(screen.id, screen.createdAt, now, screen.cohorts, Map.empty, month, formed(screen))

  def draft(
      screen: InvestmentScreen,
      now: Instant,
      month: YearMonth,
      histories: Map[Ticker, EvaluationPriceSeries]
  ): Draft =
    val through         = range(screen, month).through
    val report          = InvestmentEvaluator.evaluate(screen.id, screen.createdAt, now, screen.cohorts, histories, month, through)
    val absent          = tickers(screen).filterNot(histories.contains)
    val missing         = report.horizons.flatMap(_.cohorts.flatMap(_.missing)).filter(ticker => histories.get(ticker).exists(_.audited))
    val missingCalendar = Option
      .when(
        report.horizons.exists(h => h.entryDate.isEmpty || (h.status == EvaluationStatus.Incomplete && h.exitDate.isEmpty)) &&
          histories.get(InvestmentEvaluator.Benchmark).exists(_.audited)
      )(InvestmentEvaluator.Benchmark)
      .toList
    Draft(report, through, (absent ++ missing ++ missingCalendar).distinct)
