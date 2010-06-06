import sbt._

class BoxProject(info: ProjectInfo) extends DefaultWebProject(info) with hoffrocket.YuiCompressorPlugin {
   val scalatools_snapshot = "Scala Tools Snapshots" at
  "http://scala-tools.org/repo-snapshots/"

  val scalatools_release = "Scala Tools Releases" at
  "http://scala-tools.org/repo-releases/"

  val liftVersion = "2.0-scala280-SNAPSHOT"

  override def libraryDependencies = Set (
    "net.liftweb" % "lift-webkit" % liftVersion % "compile",
    "org.squeryl" %% "squeryl" % "0.9.4beta4" % "compile",
    "org.mortbay.jetty" % "jetty" % "6.1.22" % "test",
    "junit" % "junit" % "4.5" % "test",
    "org.scala-tools.testing" % "specs_2.8.0.Beta1" % "1.6.2" % "test",
    "com.h2database" % "h2" % "1.2.121" % "compile",
    "mysql" % "mysql-connector-java" % "5.0.5" % "compile"
  ) ++ super.libraryDependencies
}
