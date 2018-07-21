package csvtool

import scala.util.{Failure, Success, Try}
import scala.collection.immutable.{Seq => ImSeq}

//case class NotFoundElement(columnName:String,throwable: Throwable) extends IllegalArgumentException(s"${columnName}が見つかりませんでした",throwable)
case class NotFoundElement(columnName:String) extends IllegalArgumentException(s"${columnName}が見つかりませんでした")

class DataSet(val headers:Seq[String],data:Seq[Seq[String]]){

  val rows =  ImSeq(data.map(row=> Row(ImSeq(row:_*))):_*)
  private val headerMap = headers.zipWithIndex.toMap
  case class Row(data:Seq[String]){
    def apply(idx:Int)= data(idx)
    def apply(name:String)=get(name).getOrElse(throw new NotFoundElement(name))

    def get(idx:Int):Option[String] = if(data.isDefinedAt(idx)) Some(data(idx)) else None

    def get(name:String):Option[String] ={
      headerMap.get(name).fold(None:Option[String]){idx=>
        get(idx) //:Option[String]
      }
    }
  }

  override def toString: String = {
    s"header:\n${headers.mkString(",")}\nrows:\n${rows.map(_.data.zipWithIndex).mkString("\n")}"
  }
}
