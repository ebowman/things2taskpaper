package ie.boboco.taskpaper

import org.scalatest.{FlatSpec, Matchers}

class ProjectSpec extends FlatSpec with Matchers {

  "A model" should "ignore a project in the trash" in {
    val sql = io.Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("trashedproject.sql")).getLines().toStream
    val model = new Model(sql)
    model.projectMap.size shouldBe 1
    model.projectMap.head._2.title shouldBe "Inbox"   // always created
  }

  it should "handle a tagged project correctly" in {
    val sql = io.Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("taggedproject.sql")).getLines().toStream
    val model = new Model(sql)

    model.print shouldBe
      """
        |Inbox:
        |Hiring: @Management-JF
        |
      """.stripMargin.trim + "\n"
  }
}
