package stockschecker.domain

object errors {
  sealed trait AppError extends Throwable:
    def message: String
    override def getMessage: String = message

  object AppError:
    sealed trait Unauth        extends AppError
    sealed trait NotFound      extends AppError
    sealed trait Conflict      extends AppError
    sealed trait BadReq        extends AppError
    sealed trait Forbidden     extends AppError
    sealed trait Unprocessable extends AppError
    sealed trait BadData       extends AppError

    final case class FailedValidation(message: String)                   extends Unprocessable
    final case class Http(status: Int, message: String)                  extends AppError
    final case class Critical(message: String)                           extends AppError

    final case class EntityDoesNotExist(entityName: String, id: String) extends NotFound:
      override val message: String = s"$entityName with id $id does not exist"
    
    final case class StockNotFound(ticker: Ticker) extends NotFound:
      override val message: String = s"Couldn't not find stock for $ticker"
    
    final case class CompanyProfileNotFound(ticker: Ticker) extends NotFound:
      override val message: String = s"Couldn't not find company profile for $ticker"

    final case class JsonParsingFailure(original: String, error: String) extends AppError:
      override val message: String = s"Failed to parse json response: $error\n$original"
}
