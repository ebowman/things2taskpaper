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

    topLevelTasks.size shouldBe 2
    topLevelTasks.map(_.title).toSet shouldBe Set("One-Page PM 30 day trial ", "Optimizing for iteration speed")
    topLevelTasks.filter(_.tags.contains("someday")).map(_.title) shouldBe Seq("Optimizing for iteration speed")

    topLevelProjects.map(_.title).toSet shouldBe Set("Maria Topics", "Inbox")

    tasksPerArea.size shouldBe 1
    tasksPerArea.head._1.title shouldBe "[Z : Development]"
    tasksPerArea.head._2.size shouldBe 1
    tasksPerArea.head._2.head.title shouldBe "Read Linear Horn paper"

    projectsPerArea.size shouldBe 1
    projectsPerArea.head._1.title shouldBe "[Z : Development]"
    projectsPerArea.head._2.size shouldBe 2
    projectsPerArea.head._2.map(_.title).toSet shouldBe Set("Development", "My Reports' Development Maps")
    projectsPerArea.head._2.map(_.tags).filter(_.nonEmpty) shouldBe Seq(Set("done"))

    headings.map(_.title) shouldBe Seq("Self Improvement")

    tasksPerHeading.size shouldBe 1
    tasksPerHeading.head._1.title shouldBe "Self Improvement"
    tasksPerHeading.head._2.size shouldBe 1
    tasksPerHeading.head._2.map(_.project).toSet.flatten shouldBe Set(tasksPerHeading.head._1.uuid)

    println(model.print)
  }
}
