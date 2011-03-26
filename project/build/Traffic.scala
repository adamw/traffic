import sbt._

class TrafficProject(info: ProjectInfo) extends ParentProject(info) {
  val scalaToolsSnapshots = ScalaToolsSnapshots
  val databinderRepository = "Databinder Repository" at "http://databinder.net/repo/"

  lazy val traffic_ui = project("traffic-ui", "traffic-ui", info => new TrafficUI(info))

  class TrafficUI(info: ProjectInfo) extends DefaultProject(info) {
    val spde = "us.technically.spde" % ("spde-core_" + buildScalaVersion) % "0.3.1"
    // TODO: update to 2.8.1 when available
    val time = "org.scala-tools" % "time" % "2.8.0-SNAPSHOT-0.2-SNAPSHOT"

    val specs = "org.scala-tools.testing" % ("specs_" + buildScalaVersion) % "1.6.7" % "test"
    val junit = "junit" % "junit" % "4.7" % "test->default,optional"
  }
}