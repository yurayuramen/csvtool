package csvtool

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory
import com.fasterxml.jackson.module.scala.DefaultScalaModule

import scala.collection.immutable.ListMap

object CSVJacksonHelper{

  def json()={
    new CSVJacksonHelper(new ObjectMapper().registerModule(DefaultScalaModule))
  }

  def yaml()={
    new CSVJacksonHelper(new ObjectMapper(new YAMLFactory()).registerModule(DefaultScalaModule))

  }

}

class CSVJacksonHelper(val objectMapper:ObjectMapper) {

  def toSeq(dataSet:DataSet)={
    val headers = dataSet.headers
    dataSet.rows.map{row=>
      ListMap(headers.map{name=> name -> row(name)}:_*)
    }
  }

  def toString(dataSet:DataSet)={
    objectMapper.writeValueAsString(toSeq(dataSet))
  }

  def toBytes(dataSet:DataSet)={
    objectMapper.writeValueAsBytes(toSeq(dataSet))
  }

}
