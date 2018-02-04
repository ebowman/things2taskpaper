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


case class TmProject(uuid: String, title: String, tags: Set[String], area: Option[String], project: Option[String])

object TmProject {

  def toProject(line: String, tags: Map[String, Set[TmTag]], areas: Map[String, TmArea]): Option[TmProject] = {

    val parsed = TmTask.parseTask(line, areas)

    def strOpt(s: String): Option[String] = if (s.trim.isEmpty) None else Some(s)

    if (parsed(4) == "1")
      Some(TmProject(uuid = parsed.head, title = parsed(5), tags = tags.getOrElse(parsed.head, Set.empty).map(_.title),
        area = strOpt(areas.get(parsed(15)).map(_.title).getOrElse("")), project = None))
    else if (parsed(4) == "2") // heading
      Some(TmProject(uuid = parsed.head, title = parsed(5), tags = tags.getOrElse(parsed.head, Set.empty).map(_.title),
        area = strOpt(areas.get(parsed(15)).map(_.title).getOrElse("")), project = Some(parsed(15))))
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

case class TmTask(uuid: String, title: String, notes: String, tags: Set[String], project: Option[String], area: Option[String], dueDate: Option[Date], someday: Boolean) {
  def print(level: Int): String = {
    val allTags: Set[String] = if (someday) tags + "someday" else tags
    val printed = (" " * level) + "- " + title + " " + allTags.map(t => s"@$t").mkString(" ") + "\n" + formatNote(level + 2)
    printed.lines.filter(_.trim.nonEmpty).mkString("\n")
  }

  def indent(str: String, level: Int): String = str.lines.map(line => (" " * level) + line).mkString("\n")

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

object TmTask extends JavaTokenParsers {

  val escapes = Map("&amp;" -> "&", "&apos;" -> "'", "&lt;" -> "<", "&gt;" -> ">", "<note xml:space=\"preserve\">" -> "", "</note>" -> "", "\\\\r" -> "")
  def parseTask(line: String, areas: Map[String, TmArea]): Seq[String] = {
    // escape any escaped quotes
    val deQuote = line.replaceAll("''", "__QUOTE__")

    // remove the conversion from "\n" to 0x0d, etc., that sqlite helpfully puts in there. We'll do that ourselves later
    val replaced = deQuote.replaceAll("replace\\(", "").replaceAll(""",'\\n',char\(10\)\)""", "").replaceAll(""",'\\r',char\(13\)\)""", "")

    // create a parser for the input lines
    def lineParser: Parser[Seq[String]] = {
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

      ("INSERT INTO TMTask VALUES(" ~> rep1sep(token, ",")) <~ ");"
    }

    val parsed = parseAll(lineParser, replaced) match {
      case s@Success(_, _) => s.get.asInstanceOf[Seq[String]]
      case f@NoSuccess(msg, _) => sys.error(s"Could not parse: $f")
    }

    parsed.map(_.replaceAll("__QUOTE__", "'"))
  }

  def toTask(line: String, tags: Map[String, Set[TmTag]], areas: Map[String, TmArea]): Option[TmTask] = {

    val parsed = parseTask(line, areas)

    def strOpt(s: String): Option[String] = if (s.trim.isEmpty) None else Some(s)

    if (parsed(4) == "0" && parsed(3) == "0" && parsed(9) == "0")   // task and not in the trash and status = 0. no idea what status means todo
      Some(TmTask(parsed.head, parsed(5), parsed(6), tags.getOrElse(parsed.head, Set.empty).map(_.title),
        project = strOpt(parsed(16)).orElse(strOpt(parsed(24))),
        area = strOpt(areas.get(parsed(15)).map(_.title).getOrElse("")),
        dueDate = None,   // todo
        someday = parsed(11) == "2"))
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

object TmTag extends RegexParsers {
  private def tmTagParser[TmTag] = {
    val replacements = Map(
      "⨀" -> "time",
      "!!" -> "priority(1)",
      "Medium" -> "priority(2)",
      "High" -> "priority(1)",
      "Low" -> "priority(3)",
      "♨" -> "energy")

    def fix(title: String) = replacements.getOrElse(title, title).replaceAll(" ", "-")

    (("INSERT INTO TMTag VALUES('" ~> "[\\w-]+".r) ~ ("','" ~> "[^']+".r)) <~ "'.*".r ^^ { case uuid ~ title => TmTag(uuid, fix(title)) }
  }

  def tagMap(sql: Seq[String]): Map[String, TmTag] = (for {
    tag <- sql if tag.startsWith("INSERT INTO TMTag ")
  } yield {
    val t = parseAll(tmTagParser, tag).get
    t.uuid -> t
  }).toMap
}

case class TmArea(uuid: String, title: String)

object TmArea extends RegexParsers {
  private def tmAreaParser[TmArea] =
    (("INSERT INTO TMArea VALUES('" ~> "[\\w-]+".r) ~ ("','" ~> "[^']+".r)) <~ "'.*".r ^^ { case uuid ~ title => TmArea(uuid, title) }

  def areaMap(sql: Seq[String]): Map[String, TmArea] = (for {
    tag <- sql if tag.startsWith("INSERT INTO TMArea ")
  } yield {
    val t = parseAll(tmAreaParser, tag).get
    t.uuid -> t
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

object SQL extends App with RegexParsers {
  val sql = io.Source.fromFile("/Users/ebowman/things.sql").getLines().toStream


  val areaMap: Map[String, TmArea] = TmArea.areaMap(sql)
  val taskToTagMap: Map[String, Set[TmTag]] = TmTaskTag.taskToTags(sql, TmTag.tagMap(sql))
  val projectMap: Map[String, TmProject] = TmProject.projectMap(sql, taskToTagMap, areaMap)
  val tasks: Seq[TmTask] = TmTask.taskMap(sql, taskToTagMap, areaMap)

  tasks.filter(_.title.contains("Overcommitted")).foreach(println)

  val topLevelTasks = tasks.filter(_.project.isEmpty).filter(_.area.isEmpty).foreach(task => println(task.print(0)))
}
