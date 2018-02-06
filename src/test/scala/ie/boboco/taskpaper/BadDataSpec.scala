package ie.boboco.taskpaper

import org.scalatest.{FlatSpec, Matchers}

/*
These test various assertions and failures that would show up if there was a bug in Things or otherwise bad data.
 */
class BadDataSpec extends FlatSpec with Matchers {

  "The parser" should "throw an exception on unparseable sql" in {
    intercept[RuntimeException] {
      val sql = io.Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("malformed.sql")).getLines().toStream
      val model = new Model(sql)
    }
  }
}
