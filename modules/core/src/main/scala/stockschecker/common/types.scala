package stockschecker.common

import io.circe.{Decoder, Encoder}

object types {
  transparent trait StringType[Str]:
    def apply(str: String): Str = str.asInstanceOf[Str]

    given Encoder[Str] = Encoder[String].contramap(_.value)
    given Decoder[Str] = Decoder[String].map(apply)

    extension (str: Str) def value: String = str.asInstanceOf[String]
}
