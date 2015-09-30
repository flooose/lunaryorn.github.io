load.ivy("org.yaml" % "snakeyaml" % "1.16")
load.ivy("joda-time" % "joda-time" % "2.8.2")

@

import org.yaml.snakeyaml.Yaml
import ammonite.ops._
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat
import scala.collection.JavaConverters._
import scala.util.Try

val yaml = new Yaml()
val formatter = ISODateTimeFormat.date()

def renamePostWithPublishedDate(file: Path): Unit = {
  println(s"<- ${file}")
  val content = read! file
  val published = for {
    documents <- Try { yaml.loadAll(content) }.toOption
    options <- Try { documents.asScala.headOption }.toOption.flatten.map(
      _.asInstanceOf[java.util.LinkedHashMap[String, Any]])
    published <- options.asScala.get("published").map(_.asInstanceOf[java.util.Date])
  } yield new DateTime(published)

  published foreach { d =>
    val target = file / up / s"${formatter.print(d)}-${file.last}"
    println(s"${file} -> ${target}")
    mv(file, target)
  }
}

def cleanupHeaders(file: Path): Unit = {
  val content = read.lines(file)
  if (content.headOption.exists(_ == "---")) {
    println(s"Cleaning ${file.last}")
    write.over(file, content.filter(l => !l.startsWith("published: ") && !l.startsWith("tags: ")) ++ Seq(""))
  }
}

def fixupCodeBlocks(file: Path): Unit = {
  val content = read.lines(file)
  write.over(file, content.map(_.replace("```commonlisp", "```cl")) ++ Seq(""))
}
