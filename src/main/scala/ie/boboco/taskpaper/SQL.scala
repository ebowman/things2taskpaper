package ie.boboco.taskpaper

import java.sql.Date

import scala.util.parsing.combinator.{JavaTokenParsers, RegexParsers}

/*
CREATE TABLE IF NOT EXISTS 'TMTask' (
00 'uuid' TEXT PRIMARY KEY,
01 'userModificationDate' REAL,
02 'creationDate' REAL,
03 'trashed' INTEGER,
04 'type' INTEGER,        // 0 is a task, 1 is a project
05 'title' TEXT,
06 'notes' TEXT,
07 'dueDate' REAL,
08 'dueDateOffset' INTEGER,
09 'status' INTEGER,
10 'stopDate' REAL,
11 'start' INTEGER,
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
24 'actionGroup' TEXT,
25 'untrashedLeafActionsCount' INTEGER,
26 'openUntrashedLeafActionsCount' INTEGER,
27 'checklistItemsCount' INTEGER,
28 'openChecklistItemsCount' INTEGER,
'startBucket' INTEGER,
'alarmTimeOffset' REAL,
'lastAlarmInteractionDate' REAL,
'todayIndexReferenceDate' REAL,
'nextInstanceStartDate' REAL,
'dueDateSuppressionDate' REAL); */

case class TmTask(uuid: String, title: String, notes: String, tags: Set[String], project: Option[String], area: Option[String], dueDate: Option[Date])

case class TmProject(uuid: String, title: String, tags: Set[String], area: Option[String])

object TmProject {

  def toProject(line: String, tags: Map[String, Set[TmTag]], areas: Map[String, TmArea]): Option[TmProject] = {

    val parsed = TmTask.parseTask(line, tags, areas)

    def strOpt(s: String): Option[String] = if (s.trim.isEmpty) None else Some(s)

    if (parsed(4) == "1")
      Some(TmProject(uuid = parsed.head, title = parsed(5), tags = tags.getOrElse(parsed.head, Set.empty).map(_.title),
        area = strOpt(areas.get(parsed(15)).map(_.title).getOrElse(""))))
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

object TmTask extends JavaTokenParsers {

  def parseTask(line: String, tags: Map[String, Set[TmTag]], areas: Map[String, TmArea]): Seq[String] = {
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

    parseAll(lineParser, replaced) match {
      case s@Success(_, _) => s.get.asInstanceOf[Seq[String]]
      case f@NoSuccess(msg, _) => sys.error(s"Could not parse: $f")
    }
  }

  def toTask(line: String, tags: Map[String, Set[TmTag]], areas: Map[String, TmArea]): Option[TmTask] = {

    val parsed = parseTask(line, tags, areas)

    def strOpt(s: String): Option[String] = if (s.trim.isEmpty) None else Some(s)

    if (parsed(4) == "0")
      Some(TmTask(parsed.head, parsed(5), parsed(6), tags.getOrElse(parsed.head, Set.empty).map(_.title),
        project = strOpt(parsed(16)), area = strOpt(areas.get(parsed(15)).map(_.title).getOrElse("")), dueDate = None))
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
  val tagMap: Map[String, Set[TmTag]] = TmTaskTag.taskToTags(sql, TmTag.tagMap(sql))
  val tasks = TmTask.taskMap(sql, tagMap, areaMap)
  tasks.filter(_.tags.nonEmpty).filter(_.project.nonEmpty).foreach(println)
}
