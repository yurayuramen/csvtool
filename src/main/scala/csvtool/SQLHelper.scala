package csvtool

object SQLHelper{
  def apply()=new SQLHelper

}


class SQLHelper(insertBatchSize:Int = 5,nullWord:Option[String]=None) {

  private[this] val Null = "NULL"

  private[this] def buildLiteral(source:String)={
    def replace() = s"'${source.replace("'","''")}'"
    nullWord.fold(replace()){nullWord=>
      if(source == nullWord) Null else replace()
    }
  }

  def makeInsertSQLs(table:String,dataSet:DataSet)={

    val headers = dataSet.headers

    val sqlInsert = s"INSERT INTO ${table} ( ${headers.mkString(",")} )VALUES"

    dataSet.rows.map{row=>

      s"(${headers.map{name=>
        buildLiteral(row(name))
      }.mkString(", ")})"

    }.zipWithIndex.groupBy{case(_,idx)=>(idx - idx % insertBatchSize) / insertBatchSize}.toSeq.sortBy{case(key,_)=> key}.map{case(_,seq) =>
      s"""${sqlInsert}
         |${seq.map{case(sqlValue,_)=> sqlValue}.mkString(",\n")}
         |;""".stripMargin

    }.mkString("\n")
  }

  def makeUpdateSQLs(table:String,dataSet:DataSet,keyNames:String*)={
    val headers = dataSet.headers
    dataSet.rows.map{row=>
      val setClause = headers.map{name=> s"${name} = ${buildLiteral(row(name))}" }.mkString(",")
        val whereClause = keyNames.map{name=>
        val value = buildLiteral(row(name))
        if(value != Null)
          s"${name} = ${value}"
        else
          s"${name} IS ${Null}"
      }.mkString(" AND ")
      s"UPDATE ${table} SET ${setClause} WHERE ${whereClause};"
    }.mkString("\n")
  }

  def makeDeleteSQLs(table:String,dataSet:DataSet,keyNames:String*)={
    val headers = dataSet.headers
    dataSet.rows.map{row=>
      val whereClause = keyNames.map{name=>
        val value = buildLiteral(row(name))
        if(value != Null)
          s"${name} = ${value}"
        else
          s"${name} IS ${Null}"
      }.mkString(" AND ")
      s"DELETE FROM ${table} WHERE ${whereClause};"
    }.mkString("\n")
  }


}
