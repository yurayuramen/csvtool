package csvtool

import scala.util.{Failure, Success, Try}

case class NotFoundElement(columnName:String,throwable: Throwable) extends IllegalArgumentException(s"${columnName}が見つかりませんでした",throwable)

class DataSet(val headers:Seq[String],data:Seq[Seq[String]]){

  val rows = data.map(row=> Row(scala.collection.immutable.Seq(row:_*)))
  private val headerMap = headers.zipWithIndex.toMap
  case class Row(data:Seq[String]){

    def apply(idx:Int)= data(idx)

    def apply(name:String)={
      Try( data(headerMap(name)) )match{
        case Success(str)=>
          str
        case Failure(th)=>
          throw NotFoundElement(name,th)
      }
    }
  }
}
