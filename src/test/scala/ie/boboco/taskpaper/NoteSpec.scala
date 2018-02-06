package ie.boboco.taskpaper

import org.scalatest.{FlatSpec, Matchers}

class NoteSpec extends FlatSpec with Matchers {

  "The parser" should "parse and print anchor tags" in {
    val sql = io.Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("notewithanchor.sql")).getLines().toStream
    val model = new Model(sql)
    println(model.print)
  }
}
