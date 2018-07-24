package csvtool

import org.scalatest.{MustMatchers, WordSpec}

import scala.io.Source

class CSVReaderSpec extends WordSpec with MustMatchers{

  "CSVチェック" must {

    "ノーマル" in {

      val source =
        s"""
           |col1,col2,col3
           |1,2,3
           |4,5,6""".stripMargin

      val csvReader = new CSVReader()

      val dataSet = csvReader.parseWithHeader(Source.fromString(source))

      println(dataSet)
      dataSet.headers mustBe Seq("col1", "col2", "col3")
      dataSet.rows.size mustBe 2
      dataSet.rows.map(_.data) mustBe Seq(Seq("1", "2", "3"), Seq("4", "5", "6"))
    }

    "separator連打" in {

      val source =
        s"""
           |col1,col2,col3
           |1,,3
           |4,5,6""".stripMargin

      val csvReader = new CSVReader()

      val dataSet = csvReader.parseWithHeader(Source.fromString(source))

      println(dataSet)
      dataSet.headers mustBe Seq("col1", "col2", "col3")
      dataSet.rows.size mustBe 2
      dataSet.rows.map(_.data) mustBe Seq(Seq("1", "", "3"), Seq("4", "5", "6"))
    }

    "Quote区切りあり" in {

      val source =
        s"""
           |col1,col2,col3
           |1,"2",3
           |4,5,6""".stripMargin

      val csvReader = new CSVReader()

      val dataSet = csvReader.parseWithHeader(Source.fromString(source))

      dataSet.headers mustBe Seq("col1", "col2", "col3")
      dataSet.rows.size mustBe 2
      dataSet.rows.map(_.data) mustBe Seq(Seq("1", "2", "3"), Seq("4", "5", "6"))
    }

    "改行コードあり" in {

      val source =
        s"""
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
      dataSet.headers mustBe Seq("col1", "col2", "col3")
      dataSet.rows.size mustBe 4

      import scala.collection.immutable.Seq
      dataSet.rows.map(_.data) mustBe Seq(Seq("1", "2\naa\"aa\ncccc", "3"), Seq("4", "5", ""), Seq("7", "8", "9"), Seq("10", "", "")) //,Seq("11","",""))
    }

    "adjust機能off" in {

      val source =
        s"""
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

      dataSet.headers mustBe Seq("col1", "col2", "col3")
      dataSet.rows.size mustBe 5
      dataSet.rows.map(_.data) mustBe Seq(Seq("1", "2\naa\"aa\ncccc", "3"), Seq("4", "5", ""), Seq("7", "8", "9"), Seq("10", ""), Seq("11"))
    }

    "行頭空文字あり" in {

      val source =
        s"""
           |col1,col2,col3
           |,2,3
           |,5,6""".stripMargin

      val csvReader = new CSVReader()

      val dataSet = csvReader.parseWithHeader(Source.fromString(source))

      println(dataSet)
      dataSet.headers mustBe Seq("col1", "col2", "col3")
      dataSet.rows.size mustBe 2
      dataSet.rows.map(_.data) mustBe Seq(Seq("", "2", "3"), Seq("", "5", "6"))
    }
  }

  "transport" must{

    "ノーマル" in{
      val source = s"""
                      |col1,1,2,3
                      |col2,4,5,6
                      |col3,7,8,9""".stripMargin

      val csvReader = new CSVReader(transport = true)

      val dataSet = csvReader.parseWithHeader(Source.fromString(source))

      println(dataSet)
      dataSet.headers mustBe Seq("col1","col2","col3")
      dataSet.rows.size mustBe 3
      dataSet.rows.map(_.data) mustBe Seq(Seq("1","4","7"),Seq("2","5","8"),Seq("3","6","9"))


    }

  }

  "全行空の場合スキップ" must{
      val source = s"""
                      |col1,1,2,,3,
                      |col2,4,5,,6,
                      |col3,7,8,,9,""".stripMargin

    "false" in{

      val csvReader = new CSVReader(transport = true)

      val dataSet = csvReader.parseWithHeader(Source.fromString(source))

      println(dataSet)
      dataSet.headers mustBe Seq("col1","col2","col3")
      dataSet.rows.size mustBe 5
      dataSet.rows.map(_.data) mustBe Seq(Seq("1","4","7"),Seq("2","5","8"),Seq("","",""),Seq("3","6","9"),Seq("","",""))


    }
    "true" in{

      val csvReader = new CSVReader(transport = true,allEmptyRowSkip = true)

      val dataSet = csvReader.parseWithHeader(Source.fromString(source))

      println(dataSet)
      dataSet.headers mustBe Seq("col1","col2","col3")
      dataSet.rows.size mustBe 3
      dataSet.rows.map(_.data) mustBe Seq(Seq("1","4","7"),Seq("2","5","8"),Seq("3","6","9"))


    }

  }

  "skip機能チェック" must {

    "ノーマル" in {

      val source =
        s"""
           |,col1,,col2,col3
           |,,,,
           |,1,,2,3
           |,4,,5,6""".stripMargin

      val csvReader = new CSVReader(skipColumns = Seq(0,2),skipRows = Seq(1))

      val dataSet = csvReader.parseWithHeader(Source.fromString(source))

      println(dataSet)
      dataSet.headers mustBe Seq("col1", "col2", "col3")
      dataSet.rows.size mustBe 2
      dataSet.rows.map(_.data) mustBe Seq(Seq("1", "2", "3"), Seq("4", "5", "6"))
    }
  }

    "TSV" must {

    "ノーマル" in {

      val source =  "col1\tcol2\tcol3\n" ++
        "1\t2\t3\n" ++
        "4\t5\t6\n"

      val csvReader = new CSVReader(separator = '\t')

      val dataSet = csvReader.parseWithHeader(Source.fromString(source))

      println(dataSet)
      dataSet.headers mustBe Seq("col1", "col2", "col3")
      dataSet.rows.size mustBe 2
      dataSet.rows.map(_.data) mustBe Seq(Seq("1", "2", "3"), Seq("4", "5", "6"))
    }

    "セパレータ連打" in {

      val source =  "col1\tcol2\tcol3\n" ++
        "1\t\t3\n" ++
        "4\t5\t6\n"

      val csvReader = new CSVReader(separator = '\t')

      val dataSet = csvReader.parseWithHeader(Source.fromString(source))

      println(dataSet)
      dataSet.headers mustBe Seq("col1", "col2", "col3")
      dataSet.rows.size mustBe 2
      dataSet.rows.map(_.data) mustBe Seq(Seq("1", "", "3"), Seq("4", "5", "6"))
    }

  }

}
