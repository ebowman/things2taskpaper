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
    clItems.size shouldBe 3

    model.print shouldBe
      """
        |- Checklist sample @unfiled
        |  - item 1
        |  - item 2
        |  - item 3
        |Inbox:
      """
      .stripMargin.trim + "\n"
  }
}
