package improving
package jar

object Compare {
  def dirs(d1: Directory, d2: Directory) = {
    val files = d1.files filter Path.isJarOrZip toList
    val pairs = files collect { case f1 if d2 / f1.name exists => (f1, d2 /f1 toFile) }
    val names = pairs map { case (x, _) => x.name } mkString ", "
    
    println("Comparing: " + names)
    pairs foreach { case (f1, f2) => jars(f1, f2) }
  }
  
  def jars(f1: File, f2: File) = {
    val lhs       = new JarDisassembly(f1)
    val rhs       = new JarDisassembly(f2)
    val missing   = new ListBuffer[String]
    val identical = new ListBuffer[String]
    val changed   = new ListBuffer[(String, List[String])]
    
    for ((k, v) <- lhs) {
      if (rhs contains k) {
        val xs1 = v.sigs
        val xs2 = rhs(k).sigs
        
        if (xs1 == xs2) identical += k
        else changed += ((k, xs1 ++ List("<<<<>>>>") ++ xs2))
      }
      else missing += k
    }
    
    println(identical.size  + " entries identical in both jars.")
    println(missing.size + " entries in old but not new:")
    missing foreach println
    println(changed.size + " entries with altered signatures:")
    changed foreach { case (name, lines) => println(name) }
  }

  // def diff[T: Ordering](f: String => List[T])(s1: String, s2: String): (List[T], List[T]) = {
  //   val xs1 = f(s1)
  //   val xs2 = f(s2)
  //   
  //   val lhs = (xs1.toSet -- xs2).toList.sorted
  //   val rhs = (xs2.toSet -- xs1).toList.sorted
  //   
  //   (lhs, rhs)
  // }
    
}
