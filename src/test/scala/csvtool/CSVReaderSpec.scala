package csvtool

import org.scalatest.{MustMatchers, WordSpec}

import scala.io.Source

class CSVReaderSpec extends WordSpec with MustMatchers{

  "CSVチェック" must{

    "ノーマル" in{

      val source = s"""
         |col1,col2,col3
         |1,2,3
         |4,5,6""".stripMargin

      val csvReader = new CSVReader()

      val dataSet = csvReader.parseWithHeader(Source.fromString(source))

      println(dataSet)
      dataSet.headers mustBe Seq("col1","col2","col3")
      dataSet.rows.size mustBe 2
      dataSet.rows.map(_.data) mustBe Seq(Seq("1","2","3"),Seq("4","5","6"))
    }

    "Quote区切りあり" in{

      val source = s"""
                      |col1,col2,col3
                      |1,"2",3
                      |4,5,6""".stripMargin

      val csvReader = new CSVReader()

      val dataSet = csvReader.parseWithHeader(Source.fromString(source))

      dataSet.headers mustBe Seq("col1","col2","col3")
      dataSet.rows.size mustBe 2
      dataSet.rows.map(_.data) mustBe Seq(Seq("1","2","3"),Seq("4","5","6"))
    }

    "改行コードあり" in{

      val source = s"""
                      |col1,col2,col3
                      |1,"2
                      |aa""aa
                      |cccc",3
                      |4,5,
                      |7,8,9
                      |10,
                      |""".stripMargin

      val csvReader = new CSVReader()

      val dataSet = csvReader.parseWithHeader(Source.fromString(source))

      println(dataSet)
      dataSet.headers mustBe Seq("col1","col2","col3")
      dataSet.rows.size mustBe 4

      import scala.collection.immutable.Seq
      dataSet.rows.map(_.data) mustBe Seq(Seq("1","2\r\naa\"aa\r\ncccc","3"),Seq("4","5",""),Seq("7","8","9"),Seq("10","","")) //,Seq("11","",""))
    }

    "adjust機能off" in{

      val source = s"""
                      |col1,col2,col3
                      |1,"2
                      |aa""aa
                      |cccc",3
                      |4,5,
                      |7,8,9
                      |10,
                      |11
                      |""".stripMargin

      val csvReader = new CSVReader(colSizeAdjustValue = None)

      val dataSet = csvReader.parseWithHeader(Source.fromString(source))
      println(dataSet)

      dataSet.headers mustBe Seq("col1","col2","col3")
      dataSet.rows.size mustBe 5
      dataSet.rows.map(_.data) mustBe Seq(Seq("1","2\r\naa\"aa\r\ncccc","3"),Seq("4","5",""),Seq("7","8","9"),Seq("10",""),Seq("11"))
    }
  }

}
