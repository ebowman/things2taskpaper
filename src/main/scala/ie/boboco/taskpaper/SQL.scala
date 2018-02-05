package ie.boboco.taskpaper

import java.sql.Date

import scala.util.parsing.combinator.{JavaTokenParsers, RegexParsers}

/*
CREATE TABLE IF NOT EXISTS 'TMTask' (
00 'uuid' TEXT PRIMARY KEY,
01 'userModificationDate' REAL,
02 'creationDate' REAL,
03 'trashed' INTEGER,
04 'type' INTEGER,        // 0 is a task, 1 is a project, 2 is a heading
05 'title' TEXT,
06 'notes' TEXT,
07 'dueDate' REAL,
08 'dueDateOffset' INTEGER,
09 'status' INTEGER,
10 'stopDate' REAL,
11 'start' INTEGER,       // it seems start = 2 means a "Someday" task
12 'startDate' REAL,
13 'index' INTEGER,
14 'todayIndex' INTEGER,
15 'area' TEXT,
16 'project' TEXT,
17 'repeatingTemplate' TEXT,
18 'delegate' TEXT,
19 'recurrenceRule' BLOB,
20 'instanceCreationStartDate' REAL,
21 'instanceCreationPaused' INTEGER,
22 'instanceCreationCount' INTEGER,
23 'afterCompletionReferenceDate' REAL ,
24 'actionGroup' TEXT,                      // this is a heading uuid
25 'untrashedLeafActionsCount' INTEGER,
26 'openUntrashedLeafActionsCount' INTEGER,
27 'checklistItemsCount' INTEGER,
28 'openChecklistItemsCount' INTEGER,
'startBucket' INTEGER,
'alarmTimeOffset' REAL,
'lastAlarmInteractionDate' REAL,
'todayIndexReferenceDate' REAL,
'nextInstanceStartDate' REAL,
'dueDateSuppressionDate' REAL);

Things has kind of a weird model. Areas are different than projects, they can only live at the top level,
 and projects cannot nest. But when mapped to TP, which can nest projects 2 levels deep (I think...), we can
 map it pretty well, like:

 - Task
 - Task
 Project:
  - Task
  - Task
  Heading:
    - Task    (tasks within a heading don't refer to their project, the heading refers to the project)
 Area:
  - Task
  - Task
  Project:
    - Task
    - Task
*/

trait Printable {
  def printImpl(level: Int, model: Model): String

  final def print(level: Int, model: Model): String = indent(printImpl(level, model), level)


  def indent(str: String, level: Int): String = str.lines.map(line => (" " * level) + line).mkString("\n") + "\n"
}


case class TmProject(uuid: String, title: String, tags: Set[String], area: Option[String], project: Option[String], index: Int) extends Printable {
  override def printImpl(level: Int, model: Model): String =  {
    val b = new StringBuilder
    b.append(title).append(": ").append(tags.map(t => "@" + t).mkString(" ")).append("\n")

    val myTasks = model.tasks.filter(_.project.contains(uuid))
    myTasks.foreach(task => b.append(task.print(level + 1, model)))
    b.toString()
  }
}

object TmProject {

  def toProject(line: String, tags: Map[String, Set[TmTag]], areas: Map[String, TmArea]): Option[TmProject] = {

    val parsed = TmTask.parseTask(line, areas)

    def strOpt(s: String): Option[String] = if (s.trim.isEmpty) None else Some(s)

    if (parsed(4) == "1") {
      if (parsed(9) == "3") { // completed
        Some(TmProject(uuid = parsed.head, title = parsed(5), tags = tags.getOrElse(parsed.head, Set.empty).map(_.title) + "done",
          area = strOpt(areas.get(parsed(15)).map(_.title).getOrElse("")), project = None, index = parsed(13).toInt))
      } else {
        Some(TmProject(uuid = parsed.head, title = parsed(5), tags = tags.getOrElse(parsed.head, Set.empty).map(_.title),
          area = strOpt(areas.get(parsed(15)).map(_.title).getOrElse("")), project = None, index = parsed(13).toInt))
      }
    } else if (parsed(4) == "2") // heading
      Some(TmProject(uuid = parsed.head, title = parsed(5), tags = tags.getOrElse(parsed.head, Set.empty).map(_.title),
        area = strOpt(areas.get(parsed(15)).map(_.title).getOrElse("")), project = Some(parsed(16)), index = parsed(13).toInt))
    else None
  }

  def projectMap(sql: Seq[String], tags: Map[String, Set[TmTag]], areas: Map[String, TmArea]): Map[String, TmProject] = {

    (for {
      line <- sql if line.startsWith("INSERT INTO TMTask VALUES")
    } yield {
      toProject(line, tags, areas)
    }).flatten.map(tmp => tmp.uuid -> tmp).toMap
  }
}

trait InsertParser extends JavaTokenParsers {

  def tableName: String

  // create a parser for the input lines
  private def lineParser: Parser[Seq[String]] = {
    def token: Parser[String] = {
      def strToken: Parser[String] =
        """'[^']+'""".r ^^ {
          _.init.tail
        }

      def emptyToken: Parser[String] = "__QUOTE__" ^^ { _ => "" }

      def numToken: Parser[String] = floatingPointNumber

      def nullToken: Parser[String] = "NULL" ^^ { _ => "" }

      def blobToken: Parser[String] = """X'[^']+'""".r ^^ { _ => "" }

      strToken | nullToken | numToken | emptyToken | blobToken
    }

    (s"INSERT INTO $tableName VALUES(" ~> rep1sep(token, ",")) <~ ");"
  }

  def parse(tokens: String): Seq[String] = parseAll(lineParser, tokens.replaceAll("''", "__QUOTE__")) match {
    case s@Success(_, _) => s.get.asInstanceOf[Seq[String]]
    case f@NoSuccess(_, _) => sys.error(s"Could not parse $tokens: $f")

  }
}

case class TmTask(uuid: String, title: String, notes: String, tags: Set[String], project: Option[String],
                  area: Option[String], dueDate: Option[Date], someday: Boolean, index: Int) extends Printable {
  override def printImpl(level: Int, model: Model): String = {
    val allTags: Set[String] = if (someday) tags + "someday" else tags
    val printed = "- " + title + " " + allTags.map(t => s"@$t").mkString(" ") + "\n" + formatNote(level + 2)
    printed.lines.filter(_.trim.nonEmpty).mkString("\n") + "\n"
  }

  def formatNote(level: Int): String = {
    var clean = indent(notes.replaceAll("\\\\n", "\n"), level)
    for {
      (from, to) <- TmTask.escapes
    } clean = clean.replaceAll(from, to)

    while (clean.contains("<a href")) {
      val start = clean.indexOf("<a href")
      val end = clean.indexOf("</a>", start)
      val link = clean.substring(start, end + 4)
      clean = clean.replaceAllLiterally(link, processLink(link))
    }
    clean
  }

  def processLink(str: String): String = {
    assert(str.startsWith("<a") && str.endsWith("</a>"), s"Malformed anchor: $str")
    val firstQuote = str.indexOf("\"")
    assert(firstQuote != -1)
    val lastQuote = str.lastIndexOf("\"")
    assert(firstQuote != lastQuote)
    val href = str.substring(firstQuote + 1, lastQuote)
    val label = str.substring(lastQuote + 2, str.length - 4)
    if (href == label) href else s"$label -> $href"
  }
}

object TmTask extends InsertParser {

  val escapes = Map("&amp;" -> "&", "&apos;" -> "'", "&lt;" -> "<", "&gt;" -> ">", "<note xml:space=\"preserve\">" -> "", "</note>" -> "", "\\\\r" -> "")

  override val tableName = "TMTask"

  def parseTask(line: String, areas: Map[String, TmArea]): Seq[String] = {

    // remove the conversion from "\n" to 0x0d, etc., that sqlite helpfully puts in there. We'll do that ourselves later
    val replaced = line.replaceAll("replace\\(", "").replaceAll(""",'\\n',char\(10\)\)""", "").replaceAll(""",'\\r',char\(13\)\)""", "")

    parse(replaced).map(_.replaceAll("__QUOTE__", "'"))
  }

  def toTask(line: String, tags: Map[String, Set[TmTag]], areas: Map[String, TmArea]): Option[TmTask] = {

    val parsed = parseTask(line, areas)

    def strOpt(s: String): Option[String] = if (s.trim.isEmpty) None else Some(s)

    // parsed(9)/status 3 means 'completed' 2 means 'canceled'. value 1 doesn't appear in my Things, nor any value > 3
    if (parsed(4) == "0" && parsed(3) == "0" && parsed(9) == "0") // task and not in the trash and status = 0. no idea what status means todo
      Some(TmTask(parsed.head, parsed(5), parsed(6), tags.getOrElse(parsed.head, Set.empty).map(_.title),
        project = strOpt(parsed(16)).orElse(strOpt(parsed(24))),
        area = strOpt(areas.get(parsed(15)).map(_.title).getOrElse("")),
        dueDate = None, // todo
        someday = parsed(11) == "2",
        index = parsed(13).toInt))
    else None
  }

  def taskMap(sql: Seq[String], tags: Map[String, Set[TmTag]], areas: Map[String, TmArea]): Seq[TmTask] = {

    (for {
      line <- sql if line.startsWith("INSERT INTO TMTask VALUES")
    } yield {
      toTask(line, tags, areas)
    }).flatten
  }
}

case class TmTag(uuid: String, title: String)

object TmTag extends InsertParser {
  override val tableName = "TMTag"

  private val replacements = Map(
    "⨀" -> "time",
    "!!" -> "priority(1)",
    "Medium" -> "priority(2)",
    "High" -> "priority(1)",
    "Low" -> "priority(3)",
    "♨" -> "energy")

  private def fix(title: String) = replacements.foldLeft(title) { case (a, b) => a.replaceAll(b._1, b._2) }

  def tagMap(sql: Seq[String]): Map[String, TmTag] = (for {
    tag <- sql if tag.startsWith("INSERT INTO TMTag ")
  } yield {
    val fields = parse(fix(tag))
    val tmTag = TmTag(fields.head, fields(1).replaceAll(" ", "-"))
    tmTag.uuid -> tmTag
  }).toMap
}

case class TmArea(uuid: String, title: String, index: Int) extends Printable {
  override def printImpl(level: Int, model: Model): String = {
    val b = new StringBuilder
    b.append(title).append(":\n")
    val myTasks = model.tasksPerArea.filter(_._1.uuid == uuid).flatMap(_._2).sortBy(_.index)
    myTasks.foreach(task => b.append(task.print(level + 1, model)))
    val myProjects = model.projectsPerArea.filter(_._1.uuid == uuid).flatMap(_._2).sortBy(_.index)
    myProjects.foreach(project => b.append(project.print(level + 1, model)))
    b.toString()
  }
}

object TmArea extends InsertParser {

  override val tableName = "TMArea"

  def areaMap(sql: Seq[String]): Map[String, TmArea] = (for {
    tag <- sql if tag.startsWith("INSERT INTO TMArea ")
  } yield {
    val t = parse(tag)
    t.head -> TmArea(t.head, t(1), t(3).toInt)
  }).toMap

}

case class TmTaskTag(task: String, tag: String)

object TmTaskTag extends RegexParsers {

  /*
  CREATE TABLE IF NOT EXISTS 'TMTaskTag' ('tasks' TEXT NOT NULL, 'tags' TEXT NOT NULL);
   */

  // uuid of task to a set of tags for that task
  // tags is map of tag UUID to tag object
  def taskToTags(sql: Seq[String], tagByUUID: Map[String, TmTag]): Map[String, Set[TmTag]] = {

    // parse to pull out the tasks UUID and tags UUID
    def tmTaskTagParser[TmTaskTag] =
      (("INSERT INTO TMTaskTag VALUES('" ~>
        "[^']+".r) ~ ("','" ~> "[^']+".r)) <~ "'.*".r ^^ { case task ~ tag => TmTaskTag(task, tag) }

    // seq of task guid -> task tag
    val task2tags: Seq[(String, TmTaskTag)] = for {
      tag <- sql if tag.startsWith("INSERT INTO TMTaskTag ")
    } yield {
      val t = parseAll(tmTaskTagParser, tag).get
      t.task -> t
    }
    // create a seq of task guid -> Seq[tasktags] to turn into a map
    task2tags.groupBy(_._1). // map of task -> seq[task2tag]
      mapValues { task2tags =>
      task2tags.map { case (taskId, taskTag) =>
        tagByUUID(taskTag.tag)
      }.toSet
    }
  }

}

class Model(sql: Seq[String]) {
  val areaMap: Map[String, TmArea] = TmArea.areaMap(sql)
  val taskToTagMap: Map[String, Set[TmTag]] = TmTaskTag.taskToTags(sql, TmTag.tagMap(sql))
  val projectMap: Map[String, TmProject] = TmProject.projectMap(sql, taskToTagMap, areaMap)
  val tasks: Seq[TmTask] = TmTask.taskMap(sql, taskToTagMap, areaMap)

  val topLevelTasks = tasks.filter(_.project.isEmpty).filter(_.area.isEmpty).toList.sortBy(_.index)
  val topLevelProjects = projectMap.values.filter(_.area.isEmpty).filter(_.project.isEmpty).toList.sortBy(_.index)
  val tasksPerArea: List[(TmArea, Seq[TmTask])] = areaMap.map { case (uuid, tmArea) =>
    tmArea -> tasks.filter(task => task.area.contains(tmArea.title)).toList.sortBy(_.index)
  }.toList
  val areas: List[TmArea] = areaMap.values.toList.sortBy(_.index)
  val projectsPerArea = areaMap.map { case (uuid, tmArea) =>
    tmArea -> projectMap.values.filter(tmProj => tmProj.area.contains(tmArea.title)).toList.sortBy(_.index)
  }.toList

  val headings = projectMap.values.filter(tmProj => tmProj.project.nonEmpty).toList.sortBy(_.index)

  val tasksPerHeading = headings.map { heading =>
    heading -> tasks.filter(task => task.project.contains(heading.uuid)).toList.sortBy(_.index)
  }

  def print(): String = {
    val b = new StringBuilder
    topLevelTasks.foreach(p => b.append(p.print(0, this)))
    topLevelProjects.foreach(p => b.append(p.print(0, this)))
    areas.foreach(a => b.append(a.print(0, this)))
    b.toString()
  }
}

object SQL extends App with RegexParsers {
  val sql = io.Source.fromFile("/Users/ebowman/things.sql").getLines().toStream

  println(new Model(sql).print())
}
