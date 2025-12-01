package stockschecker.common

import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}
import mongo4cats.bson.ObjectId
import stockschecker.domain.errors.AppError
import sttp.tapir.{Codec, DecodeResult}

import scala.reflect.ClassTag

object types {
  object EnumType:
    def printKebabCase[E](e: E): String = e.toString.replaceAll("(?<=[a-z])(?=[A-Z])", "-").toLowerCase
    def printLowerCase[E](e: E): String = e.toString.toLowerCase
    def printUpperCase[E](e: E): String = e.toString.toUpperCase

  transparent trait EnumType[E: ClassTag](private val enums: () => Array[E], private val unwrap: E => String = EnumType.printKebabCase(_)):
    given Encoder[E]    = Encoder[String].contramap(unwrap(_))
    given Decoder[E]    = Decoder[String].emap(from)
    given KeyEncoder[E] = (e: E) => unwrap(e)
    given KeyDecoder[E] = (key: String) => from(key).toOption

    given Codec.PlainCodec[E] = Codec.string
      .mapDecode[E](s => from(s).fold(e => DecodeResult.Error(s, AppError.FailedValidation(e)), k => DecodeResult.Value(k)))(_.print)

    def from(kind: String): Either[String, E] =
      enums()
        .find(unwrap(_) == kind)
        .toRight(
          s"Invalid value $kind for enum ${implicitly[ClassTag[E]].runtimeClass.getSimpleName}, Accepted values: ${enums().map(unwrap).mkString(",")}"
        )

    extension (e: E) def print: String = unwrap(e)

  transparent trait IdType[Id]:
    def apply(id: String): Id   = id.asInstanceOf[Id]
    def apply(id: ObjectId): Id = apply(id.toHexString)

    given Encoder[Id] = Encoder[String].contramap(_.value)
    given Decoder[Id] = Decoder[String].map(apply)

    extension (id: Id)
      def value: String        = id.asInstanceOf[String]
      def toObjectId: ObjectId = ObjectId(value)

  transparent trait StringType[Str]:
    def apply(str: String): Str = str.asInstanceOf[Str]

    given Encoder[Str] = Encoder[String].contramap(_.value)
    given Decoder[Str] = Decoder[String].map(apply)

    extension (str: Str) def value: String = str.asInstanceOf[String]
}
