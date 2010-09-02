package improving
package jar

import compare.{ClassChange, ClassChangeReport}

object Billy {
  /** Compares the first two things it sees in dists/ */
  def main(args: Array[String]): Unit = {
    val paths = Directory("dists").dirs.toArray map (_ / "lib" path) take 2

    if (paths.size == 2) Compare.main(paths)
    else println("\n  ** Place two scala distributions in the dists/ directory.\n")
  }
}

object Compare {
  def dirs(d1: Directory, d2: Directory) = {
    val files = d1.files filter Path.isJarOrZip toList
    val pairs = files collect { case f1 if d2 / f1.name exists => (f1, d2 /f1.name toFile) }
    val names = pairs map { case (x, _) => x.name }
    
    println("Found " + names.size + " jars to compare:")
    names foreach println
    println("")
    
    pairs foreach { case (f1, f2) => jars(f1, f2) }
  }
  
  def jars(f1: File, f2: File) = {
    val lhs       = new JarDisassembly(f1)
    val rhs       = new JarDisassembly(f2)
    val missing   = new ListBuffer[String]
    val identical = new ListBuffer[String]
    val changed   = new ListBuffer[(String, List[ClassChange])]
    
    println(">> Now processing: " + lhs.size + " classes in " + f1.name + ".\n")
    
    for ((className, lhsClass) <- lhs; if(lhsClass.isPublic)) { // anything goes for non-public classes -- break into our packages at your own peril
      (rhs get className) match {
        case Some(rhsClass) =>
            ClassChangeReport(lhsClass, rhsClass) match {
              case List() => identical += className
              case changes => changed += ((className, changes))
            }
        case _ => missing += className    // TODO it's okay for a non-public class to be missing
      }
    }
    
    println("Classes identical in both jars: " + identical.size + "\n")
    if (missing.nonEmpty) {
      println("Classes missing from new jar: " + missing.size)
      missing.sorted foreach println
      println("")
    }
    if (changed.nonEmpty) {
      println("Classes with altered signatures: " + changed.size)
      for ((name, lines) <- changed sortBy (_._1)) {
        println(name)
        lines foreach (x => println("  " + x))
        println("")
      }
      println("")      
    }
  }

  def main(args: Array[String]): Unit = {
    if (args.size != 2)
      return println("Usage: Compare <path1> <path2>")

    val paths = args.toList map (x => Path(x))
    if (paths forall (_.isFile)) Compare.jars(paths(0).toFile, paths(1).toFile)
    else if (paths forall (_.isDirectory)) Compare.dirs(paths(0).toDirectory, paths(1).toDirectory)
    else println("Don't understand paths: " + paths.mkString(", "))
  }
}
