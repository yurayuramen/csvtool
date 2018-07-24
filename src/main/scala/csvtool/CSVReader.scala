package csvtool

import csvtool.CSVReader.LineFeedCodes
import csvtool.CSVReader.LineFeedCodes.LineFeedCode

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object CSVReader{

  object Statuses {
    sealed trait Status
    case object NewLine extends Status
    case object OutOfToken extends Status
    case object InToken extends Status
    case object InTokenQuote extends Status
    case object InTokenQuoteExitPending extends Status

  }

  object LineFeedCodes{
    sealed trait LineFeedCode
    case object Windows extends LineFeedCode
    case object Linux extends LineFeedCode
    case object ClassicMac extends LineFeedCode
  }

}

/***
  *
  * @param colSizeAdjustValue 列サイズが異なる行があったときの動きに関する設定
  *                           None:列サイズの調整をしない
  *                           Some:列サイズの調整をする。Someで内包している値を調整値として設定
  */
class CSVReader(colSizeAdjustValue:Option[String]=Some(""),separator:Char=',',linefeed:LineFeedCode = LineFeedCodes.Windows,transport:Boolean=false,skipColumns:Seq[Int]=Nil,skipRows:Seq[Int]=Nil,allEmptyRowSkip:Boolean=false) {

  import CSVReader.Statuses._
  import CSVReader.LineFeedCodes._

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

    val newLineChars =
      linefeed match{
        case Linux=>
          Seq('\n')
        case ClassicMac=>
          Seq('\r')
        case Windows=>
          Seq('\r','\n')
      }
    def isNewLine()(implicit aChar:Char)=newLineChars.contains(aChar)
    def isQuote()(implicit aChar:Char) = aChar == '"'
    def isSeparator()(implicit aChar:Char) = aChar == separator

    val iterator = source.toIterator


    @tailrec
    def fetch(){
      if(!iterator.hasNext) return
      implicit val aChar = iterator.next()
      status match {
        case NewLine =>
          if (!isNewLine())
          {
            currentRow = ArrayBuffer.empty
            currentToken = new StringBuilder
            if (isQuote()) status = InTokenQuote
            else if (isSeparator())  toOutOfToken()
            else {
              currentToken += aChar
              status = InToken
            }
          }
        case InToken =>
          if (isNewLine()) {
            currentRow += currentToken.toString()
            toNewLine()
          }
          else if (isSeparator())
            toOutOfToken()
          else
            currentToken += aChar
        case InTokenQuote =>
          if (isQuote())
            status = InTokenQuoteExitPending
          else
            currentToken += aChar
        case InTokenQuoteExitPending =>
          if (isNewLine()) {
            currentRow += currentToken.toString()
            toNewLine()
          }
          else if (isSeparator())
            toOutOfToken()
          else if (isQuote()) {
            currentToken += aChar
            status = InTokenQuote
          }
          else
            throw new IllegalStateException(s"quoteの次に想定外の文字が来てます:${aChar}")
        case OutOfToken =>
          if (isNewLine()) {
            currentRow += ""
            toNewLine()
          }
          else if (isSeparator())
            currentRow += ""
          else {
            currentToken = new StringBuilder
            if (isQuote())
              status = InTokenQuote
            else {
              currentToken += aChar
              status = InToken
            }
          }
      }//match
      fetch()
    }//def fetch

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
    }
    rows.toSeq.zipWithIndex.filterNot{case(_,rowIndex)=> skipRows.contains(rowIndex)}.map(_._1).map{_.zipWithIndex.filterNot{case(_,colIndex)=> skipColumns.contains(colIndex)}.map(_._1)}
  }

  private[this] def transport(source:Seq[Seq[String]]): Seq[Seq[String]] ={

    if(this.transport)
    {
      val rowSize = source.size
      val colSize = source(0).size
      (0 until colSize).map{colIndex=>
        (0 until rowSize).map { rowIndex =>
          source(rowIndex)(colIndex)
        }
      }
    }
    else
      source
  }

  private[this] def adjustColSize(rows:Seq[Seq[String]],colSize:Int)=
    colSizeAdjustValue.fold(rows)(colSizeAdjustValue=>
      rows.map {row=> row ++ (0 until colSize - row.size).map(_ => colSizeAdjustValue)}
    )



  def parseWithHeader(source:Source)={
    val rows1 = parseInternal(source)
    val rows = transport(rows1)
    val headers = rows.head
    val colSize = headers.size
    val dataRows = adjustColSize(rows.tail,colSize)

    val dataRows2=
    if(allEmptyRowSkip){
      dataRows.filterNot(_.forall(_.isEmpty))
    }
    else
      dataRows

    new DataSet(headers,dataRows2)
  }

  def parse(source:Source)={
    val rows = parseInternal(source)
    val maxColSize = rows.map(_.size).max
    val rows2 = transport(adjustColSize(rows,maxColSize))
    if(allEmptyRowSkip){
      rows2.filterNot(_.forall(_.isEmpty))
    }
    else
      rows2
  }





}
