package ie.boboco.taskpaper

import org.scalatest.{FlatSpec, Matchers}

class ChecklistSpec extends FlatSpec with Matchers {

  "A model" should "parse a simple checklist" in {
    val sql = io.Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("checklistitems.sql")).getLines().toStream
    val model = new Model(sql)

    import model._

    topLevelTasks.size shouldBe 1
    topLevelTasks.map(_.title).toSet shouldBe Set("Checklist sample")

    val clItems = model.checklistItems(topLevelTasks.head.uuid)
    clItems.size shouldBe 4

    model.print shouldBe
      """
        |- Checklist sample @unfiled
        |  - item 1
        |  - item 2
        |  - item 3
        |  - item 4 @done
        |Inbox:
      """
      .stripMargin.trim + "\n"
  }

  it should "ignore a checklist with an unknown status" in {
    val sql = io.Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("ignoreweirdchecklist.sql")).getLines().toStream
    val model = new Model(sql)
    model.checklistItems.size shouldBe 0
  }
}
