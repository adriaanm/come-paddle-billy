package improving
package jar

import java.lang.{ ClassLoader => JavaClassLoader }
import sun.tools.javap._
import scala.tools.nsc.util.ScalaClassLoader
import java.io.{ PrintWriter, ByteArrayInputStream }
import Javap._

/** Param bytecode is a name => bytecode lookup function.
 */
class Javap(val loader: ScalaClassLoader) {
  def this() = this(ScalaClassLoader.getSystemLoader)
  def this(cl: JavaClassLoader) = this(new JavaClassLoader(cl) with ScalaClassLoader)
  
  def bytecode(name: String): Array[Byte] = loader.findBytesForClassName(name)
  def optBytecode(name: String) = Some(bytecode(name)) filter (_.nonEmpty)
  
  def fromFile(fileName: String)  = {
    val f = File(fileName)
    if (f.exists) Some(new Disassembly(f.toByteArray))
    else None
  }
  def fromName(name: String) =
    optBytecode(name) map (x => new Disassembly(x))
  
  def apply(name: String) =
    fromName(name) orElse fromFile(name)
}

object Javap extends Javap() {
  def fromBytes(xs: Array[Byte]): Disassembly = new Disassembly(xs)
  
  def jar(path: String) = {
    val f = File(path)
    val source = new JarSource(f)
    new JarDisassembly(f, source.classFiles() map (x => x.name -> x.disassembly) toMap)
  }
}
