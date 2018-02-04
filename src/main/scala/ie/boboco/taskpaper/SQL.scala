package ie.boboco.taskpaper
import java.sql.Date

import scala.util.parsing.combinator.{JavaTokenParsers, RegexParsers}
/*
CREATE TABLE IF NOT EXISTS 'TMTask' (
00 'uuid' TEXT PRIMARY KEY,
01 'userModificationDate' REAL,
02 'creationDate' REAL,
03 'trashed' INTEGER,
04 'type' INTEGER,
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
case class TmTag(uuid: String, title: String)
case class TmArea(uuid: String, title: String)

object TmTask extends JavaTokenParsers {

  def toTask(line: String, tags: Map[String, Set[TmTag]], areas: Map[String, TmArea]): TmTask = {
    /*
    INSERT INTO TMTask VALUES('AFF4A270-56F6-43EC-A362-95F3A5BB2BE0',1508338031.7498509883,1506029797.6659588813,0,0,
    'Contract Testing Narrative - request for feedback',replace('<note xml:space="preserve">\n<a href="airmail://message?mail=
    eric.bowman%40zalando.de&amp;messageid=CAMbYuqS2Pbh%3DVNxZYmLJLCFHT%3D61XkfWCbTXbNhJKP7cXjsL-Q%40mail.gmail.com">
    Contract Testing Narrative - request for feedback</a></note>','\n',char(10)),NULL,0,3,1508338031.7486259937,1,1508111999.9999999999,-48341,162965,NULL,NULL,NULL,NULL,NULL,NULL,
    0,0,NULL,NULL,-1,-1,0,0,0,NULL,NULL,1508111999.9999999999,-62135769600.000000001,NULL);
     */

    val deQuote = line.replaceAll("''", "__QUOTE__")
    val replaced = deQuote.replaceAll("replace\\(", "").replaceAll(""",'\\n',char\(10\)\)""", "").replaceAll(""",'\\r',char\(13\)\)""", "")
    def strToken: Parser[String] = """'[^']+'""".r ^^ { _.init.tail }
    def emptyToken: Parser[String] = "__QUOTE__" ^^ { _ => "" }
    def numToken: Parser[String] = floatingPointNumber
    def nullToken: Parser[String] = "NULL" ^^ { _ => "" }
    def blobToken: Parser[String] = """X'[^']+'""".r ^^ { _ => "" }
    def token: Parser[String] = strToken | nullToken | numToken | emptyToken | blobToken
    def lineParser: Parser[Seq[String]] = ("INSERT INTO TMTask VALUES(" ~> rep1sep(token, ",")) <~ ");"
    val parsed: Seq[String] = parseAll(lineParser, replaced) match {
      case s@Success(_, _) => s.get.asInstanceOf[Seq[String]]
      case f@NoSuccess(msg, _) => sys.error(s"Could not parse: $f")
    }
    TmTask(parsed.head, parsed(5), parsed(6),tags.getOrElse(parsed.head, Set.empty).map(_.title), None, None, None)
  }

  def taskMap(sql: Seq[String], tags: Map[String, Set[TmTag]], areas: Map[String, TmArea]): Seq[TmTask] = {

    for {
      line <- sql if line.startsWith("INSERT INTO TMTask VALUES")
    } yield {
      toTask(line, tags, areas)
    }
  }
}

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

    (("INSERT INTO TMTag VALUES('" ~> "[\\w-]+".r) ~ ("','" ~> "[^']+".r)) <~ "'.*".r ^^ { case uuid ~ title => TmTag(uuid, fix(title))}
  }

  def tagMap(sql: Seq[String]): Map[String, TmTag] = (for {
    tag <- sql if tag.startsWith("INSERT INTO TMTag ")
  } yield {
    val t = parseAll(tmTagParser, tag).get
    t.uuid -> t
  }).toMap
}

object TmArea extends RegexParsers {
  private def tmAreaParser[TmArea] =
    (("INSERT INTO TMArea VALUES('" ~> "[\\w-]+".r) ~ ("','" ~> "[^']+".r)) <~ "'.*".r ^^ { case uuid ~ title => TmArea(uuid, title)}

  def areaMap(sql: Seq[String]): Map[String, TmArea] = (for {
    tag <- sql if tag.startsWith("INSERT INTO TMArea ")
  } yield {
    val t = parseAll(tmAreaParser, tag).get
    t.uuid -> t
  }).toMap

}

case class TmTaskTag(tag: String, task: String)

object TmTaskTag extends RegexParsers {
  private def tmTaskTagParser[TmTaskTag] =
    (("INSERT INTO TMTaskTag VALUES('" ~> "[^']+".r) ~ ("','" ~> "[^']+".r)) <~ "'.*".r ^^ { case tag ~ task => TmTaskTag(tag, task)}

  // uuid of task to a set of tags for that task
  // tags is map of tag UUID to tag object
  def taskToTags(sql: Seq[String], tagByUUID: Map[String, TmTag]): Map[String, Set[TmTag]] = {

    // seq of task guid -> task tag
    val seq: Seq[(String, TmTaskTag)] = for {
      tag <- sql if tag.startsWith("INSERT INTO TMTaskTag ")
    } yield {
      val t = parseAll(tmTaskTagParser, tag).get
      t.task -> t
    }
    // task guid -> Seq[tasktags]
    seq.groupBy(_._1).mapValues(tt => tt.map((t: (String, TmTaskTag)) => tagByUUID(t._1)).toSet)
  }

}

object SQL extends App with RegexParsers {
  val sql = io.Source.fromFile("/Users/ebowman/things.sql").getLines().toStream


  val areaMap: Map[String, TmArea] = TmArea.areaMap(sql)
  val tagMap: Map[String, Set[TmTag]] = TmTaskTag.taskToTags(sql, TmTag.tagMap(sql))
  val tasks = TmTask.taskMap(sql, tagMap, areaMap)
  tasks.foreach(println)
}
