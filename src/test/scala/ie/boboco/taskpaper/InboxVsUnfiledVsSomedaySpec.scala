package ie.boboco.taskpaper

import org.scalatest.{FlatSpec, Matchers}

class InboxVsUnfiledVsSomedaySpec extends FlatSpec with Matchers {

  "A model" should "parse the full-featured example model" in {
    val sql = io.Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("inboxvsunfiled.sql")).getLines().toStream
    val model = new Model(sql)

    import model._

    topLevelTasks.size shouldBe 2
    topLevelTasks.map(_.title).toSet shouldBe Set("OKR: everybody teaches. Set some goals for people teaching in DF (financial acumen)", "Optimizing for iteration speed")

    topLevelProjects.count(_.title == "Inbox") shouldBe 1
    val inbox = topLevelProjects.find(_.title == "Inbox").get

    val inboxTasks = tasks.filter(_.project.contains(inbox.uuid))
    inboxTasks.size shouldBe 1

    inboxTasks.head.title shouldBe "Call Dorothy"

    println(model.print())
    model.print() shouldBe
      """
        |- OKR: everybody teaches. Set some goals for people teaching in DF (financial acumen) @unfiled
        |- Optimizing for iteration speed @someday @unfiled
        |  https://erikbern.com/2017/07/06/optimizing-for-iteration-speed.html
        |Inbox:
        | - Call Dorothy
      """.stripMargin.trim + "\n"
  }
}
