package improving
package jar

import java.lang.{ ClassLoader => JavaClassLoader }
import sun.tools.javap._
import scala.tools.nsc.util.{ ScalaClassLoader, stringFromWriter }
import java.io.{ PrintWriter, ByteArrayInputStream }
import Disassembly._
import scala.collection.{ mutable, immutable, generic }

class JarDisassembly(file: File) extends Iterable[(String, Disassembly)] {
  val map: Map[String, Disassembly] =
    new JarSource(file).classFiles() map (x => x.name -> x.disassembly) toMap

  def contains(name: String) = map contains name
  def apply(name: String) = map(name)
  def get(name: String) = map.get(name)
  def iterator = map.iterator
  def keys     = map.keys
  def values   = map.values
  override def toString = file.toString
}

object JarDisassembly {
  def apply(path: String): JarDisassembly = apply(File(path))
  def apply(file: File): JarDisassembly = new JarDisassembly(file)
}

abstract class MemberDisassembly {
  def isMethod = false
  def isField = false
  def name: String
  def javaSig: String
  def internalSig: String
  protected def withAccess(str: String) =
    if (access.isEmpty) str
    else access :+ str mkString " "
  def signature = withAccess(name + internalSig)

  protected def access: List[String]
  lazy val isProtected: Boolean = access contains "protected"
  lazy val isDefaultAccess: Boolean = !isProtected && !isPrivate && !isPublic
  lazy val isPrivate: Boolean = access contains "private"
  lazy val isPublic: Boolean = access contains "public"
  lazy val isStatic: Boolean = access contains "static"
  lazy val isFinal: Boolean = access contains "final"

  override def toString = signature
  override def hashCode = signature.hashCode
}


class MethodDisassembly(val javaClassName: String, val data: MethodData) extends MemberDisassembly {
  override def isMethod = true

  def name      = data.getName
  def params    = data.getParameters
  lazy val access = data.getAccess.toList
  def returnTpe = data.getReturnType
  def isAbstract  = access contains "abstract"

  def javaSig = withAccess(
    if (name == "<init>") javaClassName + params
    else returnTpe + " " + name + params
  )
  def internalSig: String = data.getInternalSig
  override def equals(other: Any) = other match {
    case x: MethodDisassembly => signature == x.signature
    case _                    => false
  }
}

class FieldDisassembly(val javaClassName: String, val data: FieldData) extends MemberDisassembly {
  override def isField = true

  def name   = data.getName
  lazy val access = data.getAccess.toList
  def tpe    = data.getType
//  def isSynthetic = data.isSynthetic
  def internalSig: String = data.getInternalSig

  def javaSig = withAccess(
    tpe + " " + name
  )
  override def equals(other: Any) = other match {
    case x: FieldDisassembly  => signature == x.signature
    case _                    => false
  }
}

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
  
  //  def fields() = pwString(_.printfields())
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
  // def javaSigs = methods map signature
  // def sigs = methods map (x => x.getAccess :+ (x.getName + x.getInternalSig) mkString " ") sorted
  
  val cdata       = new ClassData(mkStream)
  def className   = cdata.getClassName
  def javaName    = javaclassname(className)
  lazy val methods = cdata.getMethods.toList map (x => new MethodDisassembly(javaName, x)) sortBy (_.name)
  lazy val fields = cdata.getFields.toList map (x => new FieldDisassembly(javaName, x)) sortBy (_.name)
  def attrs       = cdata.getAttributes.toList
  def inners      = cdata.getInnerClasses.toList
  def sourceName  = strip(cdata.getSourceName())
  def isAbstract  = cdata.getAccess contains "abstract"
  def isPublic    = cdata.getAccess contains "public"
  def isFinal     = cdata.getAccess contains "final"
  def superClassName      = cdata.getSuperClassName
  def superInterfaceNames = cdata.getSuperInterfaces

  lazy val superClass      = (Javap fromName superClassName).get
  lazy val superInterfaces = superInterfaceNames map (n => (Javap fromName n).get)

  // list of members that match m, defined/declared directly in this class or one of its transitive superclasses/superinterfaces
  // if m.isMethod => result forall _.isMethod (same thing for isField)
  def inherited(m: MemberDisassembly): List[MemberDisassembly] =
    (superClass inherited m) ++ (superInterfaces flatMap (_ inherited m)) ++ member(m)

  def member(m: MemberDisassembly): List[MemberDisassembly] =
    (methods ++ fields) filter (_.internalSig == m.internalSig)

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
    methods map (_.data) filter filt foreach (printer printMethodAttributes _)
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
