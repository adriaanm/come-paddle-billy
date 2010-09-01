import sbt._

class ComePaddleBillyProject(info: ProjectInfo) extends DefaultProject(info) {  
  override def mainClass = Some("improving.jar.Billy")
  // val scalacheck = "org.scala-tools.testing" %% "scalacheck" % "1.7" % "test" withSources()
  // val specs      = "org.scala-tools.testing" %% "specs" % "1.6.5" % "test" withSources()
} 
