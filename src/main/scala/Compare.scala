package improving
package jar

object CompareDists {
  /** Compares the first two things it sees in dists/ */
  def main(args: Array[String]): Unit = {
    val paths = Directory("dists").list.toArray filter (_.name startsWith "scala") map (x => x / "lib") take 2
    if (paths.size == 2 && (paths forall (_.exists)))
      Compare.main(paths map (_.path))
    else
      println("\n  ** Place symlinks to scala distributions in the dists/ directory.\n")
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
    val rresolve  = new ListBuffer[String]
    val identical = new ListBuffer[String]
    val changed   = new ListBuffer[(String, List[String])]
    
    println(">> Now processing: " + lhs.size + " classes in " + f1.name + ".\n")
    
    for ((k, v) <- lhs) {
      if (rhs contains k) {
        val xs1 = v.methods
        val xs2 = rhs(k).methods
        
        if (xs1 == xs2) identical += k
        else {
          val ldiff = xs1 filterNot (xs2 contains _)
          val rdiff = xs2 filterNot (xs1 contains _)
          
          if (ldiff ++ rdiff forall (_.name == "readResolve")) rresolve += k
          else {
            val tagged = (ldiff map (x => (x, true))) ++ (rdiff map (x => (x, false)))
            val combined = tagged sortBy (_._1.name) map {
              case (x, true)  => "< " + x.signature
              case (x, false) => "> " + x.signature
            }
            changed += ((k, combined))
          }
        }
      }
      else missing += k
    }
    
    println("Classes identical in both jars: " + identical.size + "\n")
    if (missing.nonEmpty) {
      println("Classes missing from new jar: " + missing.size)
      missing foreach println
      println("")
    }
    if (rresolve.nonEmpty) {
      println("Classes differing only in the readResolve() method: " + rresolve.size)
      rresolve foreach println
      println("")
    }
    if (changed.nonEmpty) {
      println("Classes with altered signatures: " + changed.size)
      for ((name, lines) <- changed) {
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
