import sbt._
import Keys._

object TrafficBuild extends Build {
	lazy val root: Project = Project("root", file(".")) aggregate(logic, ui)
	lazy val logic: Project = Project("logic", file("traffic-logic"), delegates = root :: Nil) settings(libraryDependencies ++= Seq(
			"org.scala-tools" % "time" % "2.8.0-SNAPSHOT-0.2-SNAPSHOT",
			"org.scala-tools.testing" %% "specs" % "1.6.7" % "test",
			"junit" % "junit" % "4.7" % "test->default,optional"))
	lazy val ui: Project = Project("ui", file("traffic-ui"), delegates = root :: Nil) dependsOn(logic) settings(libraryDependencies ++= Seq(
			"us.technically.spde" %% "spde-core" % "0.3.1"))
}
