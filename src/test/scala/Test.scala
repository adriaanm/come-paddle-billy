import improving.jar._

object Test {
  val defaultArgs = Array(
    "/scala/inst/28/lib/scala-library.jar",
    "/scala/inst/29/lib/scala-library.jar"
  )
  
  def main(_args: Array[String]): Unit = {
    val args = if (_args == null || _args.isEmpty) defaultArgs else _args
    if (args.size != 2)
      return println("Usage: jardiff <path1> <path2>")
    
    val paths = args.toList map (x => Path(x))
    if (paths forall (_.isFile)) Compare.jars(paths(0).toFile, paths(1).toFile)
    else if (paths forall (_.isDirectory)) Compare.dirs(paths(0).toDirectory, paths(1).toDirectory)
    else println("Don't understand paths: " + paths.mkString(", "))
  }
}
