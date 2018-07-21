package csvtool

import java.io.FileOutputStream

import scala.io.Source

object FileIO extends App{

  private def getArgs(name:String) = args.zipWithIndex.filter(_._1 == name).map{case(_,idx)=> args(idx + 1)}
  private def getArg(name:String) = getArgs(name).headOption //.getOrElse(s"${name}もしくはその値がみつかりませんでした")

  val ArgInput = "--input"
  val ArgOutput = "--output"
  val ArgFormat = "--format"

  val inputPath = getArg(ArgInput).getOrElse(throw new IllegalArgumentException(s"${ArgInput}が指定されていません"))
  val outputPath = getArg(ArgOutput).getOrElse(throw new IllegalArgumentException(s"${ArgOutput}が指定されていません"))
  val encode = getArg("--encoding").getOrElse("utf-8")

  val helper=
  getArg(ArgFormat).getOrElse("json") match{
    case "json"=>
      CSVJacksonHelper.json()
    case "yaml"=>
      CSVJacksonHelper.yaml()
  }

  Using(Source.fromFile(inputPath,encode)){source=>
    val csvReader = new CSVReader()
    val dataSet = csvReader.parseWithHeader(source)

    Using(new FileOutputStream(outputPath)){os=>
      os.write(helper.toBytes(dataSet))
      os.flush()
    }
  }
}
