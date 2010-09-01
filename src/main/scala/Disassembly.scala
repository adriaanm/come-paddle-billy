package improving
package jar

import java.lang.{ ClassLoader => JavaClassLoader }
import sun.tools.javap._
import scala.tools.nsc.io.File
import scala.tools.nsc.util.{ ScalaClassLoader, stringFromWriter }
import java.io.{ PrintWriter, ByteArrayInputStream }
import Disassembly._

class Disassembly(val bytes: Array[Byte]) {
  def mkStream  = new ByteArrayInputStream(bytes)  
  val pw        = new PrintWriter(System.out, true)
  val printer   = new JavapPrinter(mkStream, pw, newJavapEnv)
  import printer._
  
  def lines(s: String) = s split '\n'
  def pwString(f: JavapPrinter => Unit) = {
    val printer = new JavapPrinter(mkStream, pw, newJavapEnv)
    lines(stringFromWriter { pw =>
      f(new JavapPrinter(mkStream, pw, newJavapEnv))
    })
  }
  
  def signature(method: MethodData) = {
    val sig =
      if (method.getName == "<init>") javaName + method.getParameters
      else method.getReturnType + " " + method.getName + method.getParameters

    method.getAccess.toList :+ sig mkString " "
  }
  
  def fields() = pwString(_.printfields())
  // asInstanceOf          getAccess             getArgumentlength
  // getAttributes         getCode               getCodeAttributes
  // getInternalSig        getMaxLocals          getMaxStack
  // getName               getParameters         getReturnType
  // getStackMap           getStackMapTable      get_exc_index_table
  // getexception_table    getlin_num_tb         getloc_var_tb
  // getloc_var_tbsize     getnumlines           isDeprecated
  // isInstanceOf          isStatic              isSynthetic
  // read                  readCode              readExceptions
  // toString
  
  // def signatures = methods map (x => x.getName + x.getInternalSig)
  // def signatures = methods map (x => x.getName + x.getInternalSig)
  def javaSigs = methods map signature sorted
  def sigs = methods map (x => x.getAccess :+ (x.getName + x.getInternalSig) mkString " ") sorted
  
  val cdata       = new ClassData(mkStream)
  def className   = cdata.getClassName
  def javaName    = javaclassname(className)
  def methods     = cdata.getMethods.toList
  def attrs       = cdata.getAttributes.toList
  def inners      = cdata.getInnerClasses.toList
  def sourceName  = strip(cdata.getSourceName())
  
  def showFields()        = printfields()
  def showHeader()        = printclassHeader()
  def showInnerClasses()  = printInnerClasses()
  def showMethods()       = printMethods()
  def showPool()          = printcp()
  
  def show(): Unit = {
    showHeader()
    showPool()
    showMethods()
    showFields()
  }
  
  def show(filt: MethodData => Boolean): Unit = {
    methods filter filt foreach (printer printMethodAttributes _)
  }
  
  private def strip(s: String): String =
    if (s.nonEmpty && s.head == '"' && s.last == '"') s drop 1 dropRight 1 else s
  
  override def toString = "Javap(%s / %s bytes)".format(className, bytes.size)
}

object Disassembly {
  private val fieldNames    = List("showDisassembled", "showVerbose", "showInternalSigs")
  
  def newJavapEnv = {
    val env = new JavapEnvironment()
    fieldNames foreach { name =>
      val x = classOf[JavapEnvironment] getDeclaredField name
      x setAccessible true
      x.set(env, true)
    }
    env
  }
}
