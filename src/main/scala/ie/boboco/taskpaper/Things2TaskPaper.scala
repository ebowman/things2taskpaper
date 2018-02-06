package ie.boboco.taskpaper

import java.io.{File => JFile, FileWriter, PrintWriter}
import at.loveoneanother.schale.Shell
import better.files._


object Things2TaskPaper extends App {

  def options(opts: Map[String, String], argList: List[String]): Map[String, String] = {
    argList match {
      case "--thingsdb" :: db :: tail => options(opts + ("db" -> db), tail)
      case "-i" :: db :: tail => options(opts + ("db" -> db), tail)
      case "--output" :: out :: tail => options(opts + ("out" -> out), tail)
      case "-o" :: out :: tail => options(opts + ("out" -> out), tail)
      case _ :: tail => options(opts, tail)
      case Nil => opts
    }
  }

  val home = System.getProperty("user.home")
  val defaultDb = s"$home/Library/Containers/com.culturedcode.ThingsMac/Data/Library/Application\\ Support/Cultured\\ Code/Things/Things.sqlite3"
  val defaultOutput = "things.taskpaper"
  val opts = options(Map("db" -> defaultDb, "out" -> defaultOutput), args.toList)

  val tmpFile = JFile.createTempFile(".things2taskpaper", ".sql")
  tmpFile.deleteOnExit()

  val dumper = Shell(s"sqlite3 ${opts("db")}")
  dumper.input(s".output ${tmpFile.getAbsolutePath}\n", ".dump\n", ".exit\n")
  for (line <- dumper.stdout) {
    println(line)
  }
  for (line <- dumper.stderr) {
    println(line)
  }
  dumper.waitFor()
  val sql = io.Source.fromFile(tmpFile).getLines().toStream

  for {
    output <- new PrintWriter(new FileWriter(new JFile(opts("out")))).autoClosed
  } output.println(new Model(sql).print)
}
