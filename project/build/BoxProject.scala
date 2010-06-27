import sbt._

class BoxProject(info: ProjectInfo) extends DefaultWebProject(info) with hoffrocket.YuiCompressorPlugin {
  val snapshots = ScalaToolsSnapshots

  val liftVersion = "2.0-scala280-SNAPSHOT"

  override val compileOptions = Seq(Deprecation, Unchecked, Optimise, ExplainTypes)

  override def libraryDependencies = Set (
    "net.liftweb" % "lift-webkit" % liftVersion % "compile",
    "org.squeryl" %% "squeryl" % "0.9.4beta6" % "compile",
    "com.googlecode.scalaz" %% "scalaz-core" % "5.0-M3-SNAPSHOT" % "compile",
    "javax.servlet" % "servlet-api" % "2.4" % "provided",
    "org.scala-tools.testing" %% "specs" % "1.6.5-SNAPSHOT" % "test",
    "org.eclipse.jetty" % "jetty-webapp" % "7.0.2.RC0" % "test",
    "com.h2database" % "h2" % "1.2.121" % "compile",
    "mysql" % "mysql-connector-java" % "5.0.5" % "compile"
  ) ++ super.libraryDependencies
}
