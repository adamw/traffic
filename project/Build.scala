import sbt._
import Keys._

object Resolvers {
  val databinder = "Databinder Repository" at "http://databinder.net/repo/"

  val trafficResolvers = Seq(ScalaToolsSnapshots, databinder)
}

object BuildSettings {
  import Resolvers._

  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization  := "pl.softwaremill",
    version       := "1.0",
    scalaVersion  := "2.8.1",
    resolvers     := trafficResolvers
  )
}

object Dependencies {
  val time = "org.scala-tools" % "time" % "2.8.0-SNAPSHOT-0.2-SNAPSHOT"
  val specs = "org.scala-tools.testing" %% "specs" % "1.6.7" % "test"
  val junit = "junit" % "junit" % "4.7" % "test->default,optional"
  val spde = "us.technically.spde" %% "spde-core" % "0.3.1"
}

object TrafficBuild extends Build {
  import Dependencies._
  import BuildSettings._

  lazy val root: Project = Project("root", file("."), settings = buildSettings) aggregate(logic, ui)
  lazy val logic: Project = Project("logic", file("traffic-logic"), settings = buildSettings ++ Seq(libraryDependencies := Seq(time, specs, junit)))
  lazy val ui: Project = Project("ui", file("traffic-ui"), settings = buildSettings ++ Seq(libraryDependencies := Seq(spde))) dependsOn(logic)
}
