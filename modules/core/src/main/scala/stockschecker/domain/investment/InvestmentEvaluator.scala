package stockschecker.domain.investment

import stockschecker.domain.*

import java.time.{Instant, LocalDate, ZoneOffset}
import scala.math.BigDecimal.RoundingMode

/** Buy-and-hold evaluation of a frozen universe. Every constituent retains its original weight. */
object InvestmentEvaluator:
  val Version           = "forward-total-return-v1"
  val Benchmark: Ticker = Ticker("VTI")

  private def round(n: BigDecimal): BigDecimal                       = n.setScale(4, RoundingMode.HALF_UP)
  private def annualized(growth: BigDecimal, years: Int): BigDecimal =
    if growth <= 0 then BigDecimal(-100)
    else round(BigDecimal(math.pow(growth.toDouble, 1.0 / years) - 1) * 100)

  def evaluate(
      screenId: InvestmentScreenId,
      formedAt: Instant,
      evaluatedAt: Instant,
      cohorts: List[InvestmentCohort],
      histories: Map[Ticker, EvaluationPriceSeries],
      dataThrough: Option[LocalDate] = None
  ): InvestmentEvaluation =
    val formedDate = formedAt.atZone(ZoneOffset.UTC).toLocalDate
    val today      = dataThrough.getOrElse(evaluatedAt.atZone(ZoneOffset.UTC).toLocalDate)
    val benchmark  = histories.get(Benchmark)
    // VTI is the common US trading calendar. Entry is strictly AFTER the completed screen's date.
    val benchmarkDates =
      benchmark.toList.flatMap(_.closes.map(_.date)).distinct.sorted.filter(d => d.isAfter(formedDate) && !d.isAfter(today))
    val allCohorts =
      InvestmentCohort(CohortName.Vti, List(Benchmark), Some(InvestmentConcentration(1, 100, Map.empty, Map.empty, 1))) :: cohorts.filterNot(
        _.name == CohortName.Vti
      )
    val horizons = List(3, 5).map(evaluateHorizon(_, formedDate, today, benchmarkDates, allCohorts, histories))
    InvestmentEvaluation(
      screenId,
      evaluatedAt,
      Version,
      "Frozen cohorts; equal capital at next trading-session close; buy and hold for 3/5 years; dividends reinvested through total-return closes; missing sessions or delisting outcomes remain incomplete; no constituent removal. Cost scenarios deduct 0/25/50/100 basis points from terminal wealth. USD, before tax.",
      horizons,
      histories.values.map(_.source).toList.distinct.sortBy(_.print),
      histories.values.toList
        .sortBy(_.ticker.value)
        .map(h => EvaluationPriceEvidence(h.ticker, h.source, h.fetchedAt, h.adjustment, h.audited, h.snapshotIds)),
      dataThrough = dataThrough
    )

  private def evaluateHorizon(
      years: Int,
      formedDate: LocalDate,
      today: LocalDate,
      benchmarkDates: List[LocalDate],
      cohorts: List[InvestmentCohort],
      histories: Map[Ticker, EvaluationPriceSeries]
  ): HorizonEvaluation =
    val entry = benchmarkDates.headOption.filter(d => !d.isAfter(formedDate.plusDays(7)))
    val target = entry.getOrElse(formedDate.plusDays(1)).plusYears(years)
    if today.isBefore(target) then
      val progress = (entry, benchmarkDates.lastOption) match
        case (Some(start), Some(end)) =>
          val dates = benchmarkDates.filter(d => !d.isBefore(start) && !d.isAfter(end))
          cohorts.map { cohort =>
            val current = evaluateCohort(cohort, years, start, end, dates, histories)
            current.copy(
              status = if current.status == EvaluationStatus.Complete then EvaluationStatus.Pending else EvaluationStatus.Incomplete,
              totalReturnPercent = None,
              annualizedReturnPercent = None,
              excessReturnPercent = None,
              costs = Nil,
              observedThrough = Some(end),
              observedReturnPercent = current.totalReturnPercent
            )
          }
        case _ =>
          cohorts.map(c =>
            CohortEvaluation(
              c.name,
              EvaluationStatus.Pending,
              c.tickers,
              c.tickers,
              List("Entry session observations are not available yet"),
              None,
              None,
              None,
              None,
              Nil,
              concentration = c.concentration
            )
          )
      HorizonEvaluation(years, EvaluationStatus.Pending, entry, target, None, progress, List("Investment horizon has not matured"))
    else
      val exit = benchmarkDates.find(d => !d.isBefore(target)).filter(d => !d.isAfter(target.plusDays(7)))
      (entry, exit) match
        case (Some(start), Some(end)) =>
          val dates           = benchmarkDates.filter(d => !d.isBefore(start) && !d.isAfter(end))
          val results         = cohorts.map(c => evaluateCohort(c, years, start, end, dates, histories))
          val benchmarkReturn = results.find(_.name == CohortName.Vti).flatMap(_.totalReturnPercent)
          val withExcess      =
            results.map(r => r.copy(excessReturnPercent = for a <- r.totalReturnPercent; b <- benchmarkReturn yield round(a - b)))
          HorizonEvaluation(
            years,
            if withExcess.forall(_.status == EvaluationStatus.Complete) then EvaluationStatus.Complete else EvaluationStatus.Incomplete,
            entry,
            target,
            exit,
            withExcess,
            Nil
          )
        case _ =>
          HorizonEvaluation(years, EvaluationStatus.Incomplete, entry, target, exit, Nil, List("Auditable benchmark entry/exit session is missing"))

  private def evaluateCohort(
      cohort: InvestmentCohort,
      years: Int,
      start: LocalDate,
      end: LocalDate,
      dates: List[LocalDate],
      histories: Map[Ticker, EvaluationPriceSeries]
  ): CohortEvaluation =
    val paths = cohort.tickers.map { ticker =>
      val result = histories.get(ticker).toRight("price history missing").flatMap { h =>
        val duplicateDates = h.closes.map(_.date).distinct.size != h.closes.size
        val errors         = h.issues ++ List(
          Option.when(!h.audited)("price history unaudited"),
          Option.when(h.adjustment != PriceAdjustment.SplitsAndDividends)("total-return adjustments unavailable"),
          Option.when(!h.completeRange.contains(start) || !h.completeRange.contains(end))("incomplete history coverage"),
          Option.when(duplicateDates)("duplicate price dates"),
          Option.when(h.ticker != ticker)("ticker mismatch")
        ).flatten
        val prices = h.closes.map(c => c.date -> c.totalReturnClose).toMap
        val daily  = dates.map(prices.get)
        if errors.nonEmpty then Left(errors.mkString("; "))
        else if daily.exists(_.isEmpty) then Left("one or more trading sessions missing (including unresolved delistings)")
        else if daily.flatten.exists(_ < 0) || daily.headOption.flatten.forall(_ <= 0) then Left("invalid total-return price")
        else Right(daily.flatten.map(_ / daily.head.get))
      }
      ticker -> result
    }
    val failures = paths.collect { case (ticker, Left(reason)) => ticker -> reason }
    if cohort.tickers.isEmpty || failures.nonEmpty then
      CohortEvaluation(
        cohort.name,
        EvaluationStatus.Incomplete,
        cohort.tickers,
        failures.map(_._1),
        failures.map { case (t, r) => s"${t.value}: $r" } ++ Option.when(cohort.tickers.isEmpty)("empty cohort"),
        None,
        None,
        None,
        None,
        Nil,
        concentration = cohort.concentration
      )
    else
      val successful    = paths.collect { case (_, Right(path)) => path }
      val portfolio     = dates.indices.map(i => successful.map(_(i)).sum / BigDecimal(successful.size)).toList
      val growth        = portfolio.last
      val (_, drawdown) = portfolio.foldLeft((BigDecimal(1), BigDecimal(0))) { case ((peak, worst), wealth) =>
        val nextPeak = peak.max(wealth)
        (nextPeak, worst.max((nextPeak - wealth) / nextPeak * 100))
      }
      val costs = List(0, 25, 50, 100).map { bps =>
        val netGrowth = (growth - BigDecimal(bps) / 10000).max(BigDecimal(0))
        EvaluationCostScenario(bps, round((netGrowth - 1) * 100), annualized(netGrowth, years))
      }
      CohortEvaluation(
        cohort.name,
        EvaluationStatus.Complete,
        cohort.tickers,
        Nil,
        Nil,
        Some(round((growth - 1) * 100)),
        Some(annualized(growth, years)),
        Some(round(drawdown)),
        None,
        costs,
        concentration = cohort.concentration
      )
