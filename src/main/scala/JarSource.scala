package improving
package jar

import java.io.InputStream
import Properties.{ javaHome }
import Path.isJarOrZip
import java.util.jar._
import scala.util.matching.Regex
import collection.JavaConversions._

class JarSource(file: File) {
  def entries()    = new Entries(new JarFile(file.jfile))
  def classFiles() = entries() filter (_.name endsWith ".class")
  def files()      = entries() filterNot (_.isDirectory)
  
  class JarFileEntry(entry: JarEntry, stream: InputStream) {
    def isDirectory = entry.isDirectory
    def name        = entry.getName
    val bytes: Array[Byte] =
      if (isDirectory) Array()
      else new Streamable.Bytes { val inputStream = stream } toByteArray
    
    def disassembly = Javap.fromBytes(bytes)
    override def toString = name
  }
  
  class Entries(val jarFile: JarFile) extends Traversable[JarFileEntry] {
    def mkEntry(x: JarEntry) = new JarFileEntry(x, jarFile getInputStream x)
    def foreach[U](f: JarFileEntry => U): Unit = {
      try jarFile.entries() filter (_ != null) foreach (x => f(mkEntry(x)))
      finally jarFile.close()
    }
  }
    
  def javaps = files() map (_.disassembly)
  
  override def toString = file.toString
}
