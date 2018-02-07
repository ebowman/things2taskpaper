package ie.boboco.taskpaper

import org.scalatest.{FlatSpec, Matchers}

/*
These test various assertions and failures that would show up if there was a bug in Things or otherwise bad data.
 */
class RecurringTaskSpec extends FlatSpec with Matchers {

  "The parser" should "parse a recurring task with a blob" in {
    val sql = io.Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("recurringtask.sql")).getLines().toStream
    val model = new Model(sql)
  }
}
