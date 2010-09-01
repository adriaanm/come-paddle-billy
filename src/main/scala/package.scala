package improving

package object jar {
  /** Missing pieces of Predef */
  def optManifest[T](implicit m: OptManifest[T]): OptManifest[T] = m
  val ClassManifest   = scala.reflect.ClassManifest
  val NoManifest      = scala.reflect.NoManifest
  val Manifest        = scala.reflect.Manifest
  
  /** Scala compiler */
  type Directory        = scala.tools.nsc.io.Directory
  type File             = scala.tools.nsc.io.File
  type Path             = scala.tools.nsc.io.Path
  type Process          = scala.tools.nsc.io.Process

  val Directory        = scala.tools.nsc.io.Directory
  val File             = scala.tools.nsc.io.File
  val Path             = scala.tools.nsc.io.Path
  val Process          = scala.tools.nsc.io.Process
  val Properties       = scala.tools.nsc.Properties
  val Streamable       = scala.tools.nsc.io.Streamable
  
  /** Scala library */
  type HashMap[A, B]  = scala.collection.mutable.HashMap[A, B]
  type HashSet[A]     = scala.collection.mutable.HashSet[A]
  type ListBuffer[T]  = scala.collection.mutable.ListBuffer[T]
  type OptManifest[T] = scala.reflect.OptManifest[T]
  
  val ListBuffer      = scala.collection.mutable.ListBuffer
  
  /** Java */
  type IOException    = java.io.IOException
  type JClass         = java.lang.Class[_]
  type JIterable[T]   = java.lang.Iterable[T]
  type JFile          = java.io.File
  type JList[T]       = java.util.List[T]
  type JMethod        = java.lang.reflect.Method
  type JReader        = java.io.Reader
  type URL            = java.net.URL  
}

