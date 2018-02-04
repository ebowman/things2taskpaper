package ie.boboco.taskpaper

import java.io._

import scala.collection.immutable
import scala.util.parsing.combinator.RegexParsers


/*
Title	Creation Date	Modification Date	Due Date	Start Date	Project	Area	Subtask	Notes
 */

case class RawTask(title: String, createdAt: String, modifiedAt: String, dueAt: String, startAt: String,
                   project: String, area: String, subtask: String, notes: String)

object RawTask {
  def mkRawTask(seq: Seq[String]): RawTask = {
    val fixed = seq.collect {
      case "__QUOT__" => ""
      case hasQuote if hasQuote.contains("__QUOT__") => hasQuote.replaceAll("__QUOT__", "\"")
      case normal => normal
    }

    def fixProject(x: String) = if (x.isEmpty) "Unfiled" else x

    fixed match {
      case List(a, b, c, d, e, f, g, h) => RawTask(a, b, c, d, e, fixProject(f), g, h, "")
      case List(a, b, c, d, e, f, g, h, i) => RawTask(a, b, c, d, e, fixProject(f), g, h, i)
      case _ => sys.error(s"Malformed task seq: $fixed")
    }
  }

  def mkTask(raw: RawTask): Task = {
    val dueAt = (raw.dueAt, raw.startAt) match {
      case ("", "") => None
      case (due, "") => Some(due)
      case ("", start) => Some(start)
      case (due, start) => Some(if (due > start) due else start)
    }
    Task(raw.title, raw.notes, dueAt)
  }

  def mkProject(name: String, allTasks: Seq[RawTask]): Project = Project(name, allTasks.filter(_.project == name).map(mkTask))
}

case class Task(name: String, notes: String, dueAt: Option[String]) {
  def formatNote(level: Int): String = {
    if (notes.lines.size > 4) {
     if (notes.lines.exists(_ contains "airmail:")) "\n" + notes.lines.filter(_.contains("airmail:")).mkString("\n")
     else "\n" + notes.lines.filterNot(_.trim.isEmpty).mkString("\n")
    } else {
      val formatted = notes.replaceAll(", ", "\n").lines.filterNot(_.trim.isEmpty).map(l => (" " * level) + l).mkString("\n")
      if (formatted.trim.isEmpty) ""
      else "\n" + formatted
    }

  }
  def str(level: Int): String = (" " * level) + "- " + name  + formatNote(level + 1)

}

case class Project(name: String, tasks: Seq[Task]) {
  def str(level: Int): String = (" " * level) + name + ":\n"  + tasks.map(_.str(level + 1)).mkString("\n")

}


//case class Subtask


object Main extends App with RegexParsers {

  def unquotedField: Parser[String] = """ [^;"]+ """.trim.r

  def quotedField: Parser[String] =
    """ "[^"]*" """.trim.r ^^ {
      _.tail.init
    }

  def field: Parser[String] = quotedField | unquotedField | ""

  def row = rep1sep(field, ";")

  def quote(str: String): String = str.replaceAll("\"\"", "__QUOT__")

  def variant1(str: String): String = str.replaceAll("\"\"\"", "\"__QUOT__")

  def variant2(str: String): String = str.replaceAll("\"\"\"", "__QUOT__\"")

  val records: Seq[RawTask] = for {
    raw <- io.Source.fromFile(new File("/Users/ebowman/things2.csv"), "UTF-8").getLines().toStream.tail // ignore headers
  } yield {
    val record: Seq[ParseResult[List[String]]] = for (line <- List(variant1(raw), variant2(raw)).map(quote)) yield {
      parseAll(row, line)
    }
    record.find(_.successful).map(field => RawTask.mkRawTask(field.get)).getOrElse(sys.error(s"Could not parse $raw"))
  }

  for {projectName <- records.map(_.project).distinct} {
    val project = RawTask.mkProject(projectName, records)
    println(project.str(0))
  }
//  for (area <- areaNames) {
//    val areas = records.filter(_.area == area).foreach(a => println())
//  }

  // convert subtasks
  //records.foreach(println)
  //records.filter(_.startAt.nonEmpty).foreach(println)
  //records.foreach(r => if (r.lengthCompare(8) == 0) println(r))
  // 9 field lists are are normal tasks
  // 8 field lists subtasks and need to be handled separately
  // the others are weird and we need to figure it out
  //if (line.count(_ == ';') == 9) println(line)

}


/** The things.sh script generates bullshit utf8 somehow, this fixes it, sort of. */
object FixUTF8 extends App {
  val input = new InputStreamReader(new FileInputStream("/Users/ebowman/things.csv"), "UTF-8")
  val output = new OutputStreamWriter(new FileOutputStream("/Users/ebowman/things2.csv"), "UTF-8")
  var next = input.read()
  while (next != -1) {
    output.write(next)
    next = input.read()
  }
  output.close()
}