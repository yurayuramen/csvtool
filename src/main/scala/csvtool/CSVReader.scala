package csvtool

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object CSVReader{

  sealed trait Status
  case object NewLine extends Status
  case object OutOfToken extends Status
  case object InToken extends Status
  case object InTokenQuote extends Status
  case object InTokenQuoteExitPending extends Status

}

/***
  *
  * @param colSizeAdjustValue 列サイズが異なる行があったときの動きに関する設定
  *                           None:列サイズの調整をしない
  *                           Some:列サイズの調整をする。Someで内包している値を調整値として設定
  */
class CSVReader(colSizeAdjustValue:Option[String]=Some("")) {

  import CSVReader._

  def parseInternal(source:Source): Seq[Seq[String]] ={




    val rows = ArrayBuffer.empty[Seq[String]]
    var currentRow:ArrayBuffer[String] = null
    var currentToken:StringBuilder = null


    var status:Status = NewLine

    def toNewLine(): Unit ={
      rows += currentRow
      currentRow = null
      currentToken = null
      status = NewLine
    }

    def toOutOfToken(): Unit ={
      currentRow += currentToken.toString()
      currentToken = null
      status = OutOfToken
    }

    val newLineChars = Seq('\r','\n')
    def isNewLine(aChar:Char)=newLineChars.contains(aChar)
    def isQuote(aChar:Char) = aChar == '"'
    def isComma(aChar:Char) = aChar == ','

    val iterator = source.toIterator


    @tailrec
    def fetch(){
      if(iterator.hasNext) {
        val aChar = iterator.next()
        status match {
          case NewLine =>
            if (isNewLine(aChar)) {
              //TODO 空行
            }
            else {
              currentRow = ArrayBuffer.empty
              currentToken = new StringBuilder
              if (isQuote(aChar)) {
                status = InTokenQuote
              }
              else {
                currentToken += aChar
                status = InToken
              }
            }
          case InToken =>
            if (isNewLine(aChar)) {

              currentRow += currentToken.toString()
              toNewLine()
            }
            else if (isComma(aChar)) {
              toOutOfToken()
            }
            else {
              currentToken += aChar
            }
          case InTokenQuote =>
            if (isQuote(aChar)) {
              status = InTokenQuoteExitPending
            }
            else {
              currentToken += aChar
            }
          case InTokenQuoteExitPending =>
            if (isNewLine(aChar)) {

              currentRow += currentToken.toString()
              toNewLine()
            }
            else if (isComma(aChar)) {
              toOutOfToken()
            }
            else if (isQuote(aChar)) {
              currentToken += aChar
              status = InTokenQuote
            }
            else {
              throw new IllegalStateException(s"quoteの次に想定外の文字が来てます:${aChar}")
            }
          case OutOfToken =>
            if (isNewLine(aChar)) {

              currentRow += ""
              toNewLine()
            }
            else {
              currentToken = new StringBuilder
              if (isQuote(aChar)) {
                status = InTokenQuote
              }
              else {
                currentToken += aChar
                status = InToken
              }
            }
        }//match
        fetch()
      }//
    }//def

    fetch()

    status match{
      case NewLine=>
      case OutOfToken=>
        currentRow += ""
        rows += currentRow
      case InToken =>
        currentRow += currentToken.toString()
        toNewLine()
      case InTokenQuoteExitPending =>
        currentRow += currentToken.toString()
        toNewLine()
      case InTokenQuote =>
        throw new IllegalStateException(s"quoteで始まった" )

    }
    rows.toSeq
  }

  def parseWithHeader(source:Source)={
    val rows = parseInternal(source)

    val headers = rows.head

    val colSize = headers.size

    val dataRows =
      colSizeAdjustValue.fold{rows.tail}{colSizeAdjustValue=>
        rows.tail.map {row=> row ++ (0 until colSize - row.size).map(_ => colSizeAdjustValue)}
      }

    new DataSet(headers,dataRows)
  }

  def parse(source:Source)={

    val rows = parseInternal(source)
    val maxColSize = rows.map(_.size).max
    colSizeAdjustValue.fold{rows}{colSizeAdjustValue=>
      rows.map {row=> row ++ (0 until maxColSize - row.size).map(_ => colSizeAdjustValue)}
    }
  }





}
