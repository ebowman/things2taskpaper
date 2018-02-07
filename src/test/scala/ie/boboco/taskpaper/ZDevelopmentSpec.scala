package ie.boboco.taskpaper

import org.scalatest._

/**
  * This tests using the zdevelopment.sql file in src/test/resources, which has some snippets from my Things 3
  * file. In particular, these snippets create an area, tasks and projects (completed and not) within that area,
  * and tasks within the projects, including headings. This is a pretty full example of the entire supported feature
  * set.
  */
class ZDevelopmentSpec extends FlatSpec with Matchers {

  "A model" should "parse the full-featured example model" in {
    val sql = io.Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("zdevelopment.sql")).getLines().toStream
    val model = new Model(sql)

    import model._

    topLevelTasks.size shouldBe 3
    topLevelTasks.map(_.title).toSet shouldBe Set("One-Page PM 30 day trial ", "Did Jeff get a parking spot?", "Optimizing for iteration speed")
    topLevelTasks.filter(_.tags.contains("someday")).map(_.title) shouldBe Seq("Optimizing for iteration speed")

    topLevelProjects.map(_.title).toSet shouldBe Set("Maria Topics", "Inbox")

    tasksPerArea.size shouldBe 2
    tasksPerArea.map(_._1.title).toSet shouldBe Set("[Z : Development]", "[P]")
    tasksPerArea.find(_._1.title == "[Z : Development]").map(_._2).get.size shouldBe 2
    tasksPerArea.head._2.map(_.title).toSet shouldBe Set("Read Linear Horn paper", "Read Linear Horn paper again")

    projectsPerArea.size shouldBe 2
    projectsPerArea.map(_._1.title).toSet shouldBe Set("[Z : Development]", "[P]")
    projectsPerArea.find(_._1.title == "[Z : Development]").map(_._2).get.size shouldBe 2
    projectsPerArea.find(_._1.title == "[Z : Development]").get._2.map(_.title).toSet shouldBe Set("Development", "My Reports' Development Maps")
    projectsPerArea.find(_._1.title == "[Z : Development]").get._2.map(_.tags).filter(_.nonEmpty) shouldBe Seq(Set("done"))

    headings.map(_.title).toSet shouldBe Set("Self Improvement", "Managing a Team")

    tasksPerHeading.size shouldBe 2
    tasksPerHeading.map(_._1.title).toSet shouldBe Set("Self Improvement", "Managing a Team")
    tasksPerHeading.find(_._1.title == "Self Improvement").get._2.size shouldBe 2
    tasksPerHeading.find(_._1.title == "Self Improvement").get._2.map(_.project).toSet.flatten shouldBe Set(tasksPerHeading.head._1.uuid)

    model.print shouldBe
      """
        |- One-Page PM 30 day trial  @unfiled
        |- Did Jeff get a parking spot? @Jeff @unfiled
        |- Optimizing for iteration speed @someday @unfiled
        |Inbox:
        |Maria Topics:
        |[Z : Development]:
        | - Read Linear Horn paper @someday
        | - Read Linear Horn paper again @someday
        | Development:
        |   - Five things everyone should know about Unicode 
        |   Self Improvement:
        |      - 10 Ways to Make Peak-State Decisions and Invest in Yourself
        |           https://medium.com/the-mission/10-ways-to-make-bold-decisions-invest-in-yourself-and-live-on-your-terms-f79d527adcd2
        |      - Think Time
        |   Managing a Team:
        | My Reports' Development Maps: @done
        |[P]:
        |
      """.stripMargin.trim + "\n"
  }
}
