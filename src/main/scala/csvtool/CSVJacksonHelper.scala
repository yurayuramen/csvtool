package csvtool

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule

import scala.collection.immutable.ListMap

object CSVJacksonHelper{

  def apply()={
    new CSVJacksonHelper(new ObjectMapper().registerModule(DefaultScalaModule))
  }

}

class CSVJacksonHelper(val objectMapper:ObjectMapper) {

  def jsonAsString(dataSet:DataSet)={

    val headers = dataSet.headers

    val seq=
    dataSet.rows.map{row=>
      ListMap(headers.map{name=> name -> row(name)}:_*)
    }
    objectMapper.writeValueAsString(seq)

  }


}
