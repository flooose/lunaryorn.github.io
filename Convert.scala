load.ivy("org.yaml" % "snakeyaml" % "1.16")

@

import org.yaml.snakeyaml.Yaml
import ammonite.ops._

val yaml = new Yaml()

def convertPost(file: Path): String = {
  val content = read! file
  val options = yaml
    .loadAll(content)
    .asScala
    .head
    .asInstanceOf[java.util.LinkedHashMap[String, Any]]
    .asScala
}
