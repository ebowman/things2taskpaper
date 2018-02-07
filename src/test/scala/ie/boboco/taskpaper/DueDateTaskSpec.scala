package ie.boboco.taskpaper

import org.scalatest.{FlatSpec, Matchers}

/**
  * Verify we can load a task with a due date and print it correctly.
  */
class DueDateTaskSpec extends FlatSpec with Matchers {

  "A @duedate" should "be parsed and printed correctly" in {
    val sql = io.Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("duedatetask.sql")).getLines().toStream
    val model = new Model(sql)
    model.print shouldBe
      """
        |- Follow up with D @someday @unfiled @due(2018-02-09)
        |Inbox:
      """.stripMargin.trim + "\n"
  }
}
