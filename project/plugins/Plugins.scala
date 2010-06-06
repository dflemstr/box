import sbt._

class Plugins(info: ProjectInfo) extends PluginDefinition(info) {
  val sbtYui = "hoffrocket" % "sbt-yui" % "0.2"
  val sonaTypeReo = "Sonatype Repo" at "http://oss.sonatype.org/content/groups/github/"
}

