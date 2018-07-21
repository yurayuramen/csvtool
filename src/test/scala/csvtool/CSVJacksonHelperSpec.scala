package csvtool

import org.scalatest.{FlatSpec, MustMatchers, WordSpec}

import scala.io.Source

class CSVJacksonHelperSpec extends WordSpec with MustMatchers{
  "parseWithHeader" must{
    "json" in {
      val source =
        s"""
           |col1,col2,col3
           |1,2,3
           |4,"5"" ",6
           |""".stripMargin
      val csvReader = new CSVReader()
      val helper = CSVJacksonHelper.json()
      val dataSet = csvReader.parseWithHeader(Source.fromString(source))
      val json = helper.toString(dataSet)
      json mustBe """[{"col1":"1","col2":"2","col3":"3"},{"col1":"4","col2":"5\" ","col3":"6"}]"""
    }
    "yaml" in {
      val source =
        s"""
           |col1,col2,col3
           |1,2,3
           |4,"5"" ",6
           |""".stripMargin
      val csvReader = new CSVReader()
      val helper = CSVJacksonHelper.yaml()
      val dataSet = csvReader.parseWithHeader(Source.fromString(source))
      val json = helper.toString(dataSet)

      println(json)

      json mustBe
         """---
          |- col1: "1"
          |  col2: "2"
          |  col3: "3"
          |- col1: "4"
          |  col2: "5\" "
          |  col3: "6"
          |""".stripMargin
    }
  }

}
