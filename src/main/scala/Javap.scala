package improving
package jar

import java.lang.{ ClassLoader => JavaClassLoader }
import sun.tools.javap._
import scala.tools.nsc.io.File
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
  // 
  // def apply(className: String): Unit  =
  //   wrap(className) foreach (_.show())
  // 
  // def apply(className: String, methodName: String): Unit =
  //   apply(className, _.getName == methodName)    
  // 
  // def apply(className: String, f: MethodData => Boolean): Unit =
  //   wrap(className) foreach (_ show f)
  // 
  // def file(fileName: String) = Some(new Wrapper(File(fileName).toByteArray))
}

object Javap extends Javap() {
  def apply(bytes: Array[Byte]): Disassembly = new Disassembly(bytes)
  def apply(name: String, cl: ScalaClassLoader): Disassembly = new Disassembly(cl.findBytesForClassName(name))
}
