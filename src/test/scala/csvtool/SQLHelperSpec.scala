package csvtool

import org.scalatest.{MustMatchers, WordSpec}

import scala.io.Source

class SQLHelperSpec extends WordSpec with MustMatchers{

  "makeInsertSQLs" must {

    "ノーマル" in {

      val source =
        s"""
           |col1,col2,col3
           |1,2,3
           |2,5,6
           |3,2,3
           |4,5,6
           |5,2,3
           |6,5,6
           |7,2,3
           |8,5,6
           |9,2,3
           |10,5,6
           |11,2,3
           |12,5,6
           |13,2,3
           |14,5,6
           |""".stripMargin

      val sqlHelper = SQLHelper()
      val csvReader = new CSVReader()

      val dataSet = csvReader.parseWithHeader(Source.fromString(source))

      println(dataSet)

      val sqls = sqlHelper.makeInsertSQLs("test",dataSet)
      println(sqls)
    }
    "カンマ、クォートを含む" in {

      val source =
        s"""
           |col1,col2,col3
           |1,2',3
           |2,"5"" ",6
           |""".stripMargin

      val sqlHelper = SQLHelper()
      val csvReader = new CSVReader()

      val dataSet = csvReader.parseWithHeader(Source.fromString(source))

      println(dataSet)

      val sqls = sqlHelper.makeInsertSQLs("test",dataSet)
      println(sqls)

      val expected =
            """INSERT INTO test ( col1,col2,col3 )VALUES
            |('1', '2''', '3'),
            |('2', '5" ', '6')
            |;""".stripMargin

      sqls mustBe expected
    }
  }

  "makeUpdateSQLs" must {

    "カンマ、クォートを含む" in {

      val source =
        s"""
           |col1,col2,col3
           |1,2',3
           |2,"5"" ",6
           |""".stripMargin

      val sqlHelper = SQLHelper()
      val csvReader = new CSVReader()

      val dataSet = csvReader.parseWithHeader(Source.fromString(source))

      println(dataSet)

      val sqls = sqlHelper.makeUpdateSQLs("test",dataSet,"col1","col3")
      println(sqls)

      val expected =
        """UPDATE test SET col1 = '1',col2 = '2''',col3 = '3' WHERE col1 = '1' AND col3 = '3';
          |UPDATE test SET col1 = '2',col2 = '5" ',col3 = '6' WHERE col1 = '2' AND col3 = '6';""".stripMargin

      sqls mustBe expected
    }
  }

  "makeDeleteSQLs" must {

    "カンマ、クォートを含む" in {

      val source =
        s"""
           |col1,col2,col3
           |1,2',3
           |2,"5"" ",6
           |""".stripMargin

      val sqlHelper = SQLHelper()
      val csvReader = new CSVReader()

      val dataSet = csvReader.parseWithHeader(Source.fromString(source))

      println(dataSet)

      val sqls = sqlHelper.makeDeleteSQLs("test",dataSet,"col1","col3")
      println(sqls)

      val expected =
        """DELETE FROM test WHERE col1 = '1' AND col3 = '3';
          |DELETE FROM test WHERE col1 = '2' AND col3 = '6';""".stripMargin

      sqls mustBe expected
    }
  }

}
