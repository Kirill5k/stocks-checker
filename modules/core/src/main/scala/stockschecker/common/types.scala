package stockschecker.common

import io.circe.{Decoder, Encoder}
import mongo4cats.bson.ObjectId

object types {
  transparent trait IdType[Id]:
    def apply(id: String): Id = id.asInstanceOf[Id]
    def apply(id: ObjectId): Id = apply(id.toHexString)
    
    given Encoder[Id] = Encoder[String].contramap(_.value)
    given Decoder[Id] = Decoder[String].map(apply)

    extension (id: Id)
      def value: String = id.asInstanceOf[String]
      def toObjectId: ObjectId = ObjectId(value)
  
  transparent trait StringType[Str]:
    def apply(str: String): Str = str.asInstanceOf[Str]

    given Encoder[Str] = Encoder[String].contramap(_.value)
    given Decoder[Str] = Decoder[String].map(apply)

    extension (str: Str) def value: String = str.asInstanceOf[String]
}
