package improving
package compare

import jar._

/**
 * Created by IntelliJ IDEA.
 * User: adriaan
 * Date: Sep 2, 2010
 * Time: 10:52:14 AM
 * To change this template use File | Settings | File Templates.
 */

trait ClassAnalyzer extends ((Disassembly, Disassembly) => List[ClassChange]) {
  def err(cond: Boolean)(msg: String) = if(cond) List(ClassChange(msg)) else List()
}

object ClassChangeReport extends ClassAnalyzer {
  val analyzers: List[ClassAnalyzer] = List()
  def apply(lhs: Disassembly, rhs: Disassembly) = analyzers flatMap (_.apply(lhs,rhs))
}

case class ClassChange(msg: String)

object analyzers {
/*13.4.1 abstract Classes

If a class that was not abstract is changed to be declared abstract, then pre-existing binaries that attempt to create new instances of that class will throw either an InstantiationError at link time, or (if a reflective method is used) an InstantiationException at run time; such a change is therefore not recommended for widely distributed classes.
Changing a class that was declared abstract to no longer be declared abstract does not break compatibility with pre-existing binaries.
*/
object Abstract extends ClassAnalyzer {
  def apply(lhs: Disassembly, rhs: Disassembly) = err(rhs.isAbstract && !lhs.isAbstract)("Must not be abstract.")
}

/*13.4.2 final Classes

If a class that was not declared final is changed to be declared final, then a VerifyError is thrown if a binary of a pre-existing subclass of this class is loaded, because final classes can have no subclasses.
Changing a class that was declared final to no longer be declared final does not break compatibility with pre-existing binaries.
*/
object Final extends ClassAnalyzer {
  def apply(lhs: Disassembly, rhs: Disassembly) = err(rhs.isFinal && !lhs.isFinal)("Must not be final.")
}

/*
13.4.3 public Classes

If a class that was declared public is changed to not be declared public, then an IllegalAccessError is thrown if a pre-existing binary is linked that needs but no longer has access to the class type.
Changing a class that was not declared public to be declared public does not break compatibility with pre-existing binaries.
*/
object Public extends ClassAnalyzer {
  def apply(lhs: Disassembly, rhs: Disassembly) = err(!rhs.isPublic && lhs.isPublic)("Must be public.")
}

/*
13.4.4 Superclasses and Superinterfaces

A ClassCircularityError is thrown at load time if a class would be a superclass of itself.
Changing the direct superclass or the set of direct superinterfaces of a class type will not break compatibility with pre-existing binaries,
provided that the total set of superclasses or superinterfaces, respectively, of the class type loses no members.

If a change to the direct superclass or the set of direct superinterfaces results in any class or interface no longer being a superclass or superinterface, respectively, then link-time errors may result if pre-existing binaries are loaded with the binary of the modified class.
*/
object Hierarchy extends ClassAnalyzer {
  def apply(lhs: Disassembly, rhs: Disassembly) =
    err(lhs.superClassName != rhs.superClassName)("Must inherit the same superclass. "+(lhs.superClassName, rhs.superClassName)) ++
    err(lhs.superInterfaces != rhs.superInterfaces)("Must inherit the same superinterfaces. "+(lhs.superInterfaces, rhs.superInterfaces)) // TODO: this may be relaxed: allowed to add superinterfaces (but: abstract method errors!)
}

/*13.4.5 Class Body and Member Declarations

*/
object ClassBody extends ClassAnalyzer {
  def byNameDiff[T <: MemberDisassembly](as: List[T], bs: List[T]): List[T] = as filter (a => !bs.exists(b => a.name == b.name))
  def sameNameDiff[T <: MemberDisassembly](ms: List[T], m: T): List[T] = ms filter (a => a.name == m.name)
  def apply(lhs: Disassembly, rhs: Disassembly) = {
    val lhsMethods = lhs.methods
    val rhsMethods = rhs.methods
    val lhsFields = lhs.fields
    val rhsFields = rhs.fields
    if(lhsMethods == rhsMethods && lhsFields == rhsFields) List()
    else {
      val deletedMethods = byNameDiff(lhsMethods, rhsMethods)
      val deletedNonPrivateMethods = deletedMethods filter (m => m.access != "private")
      val deletedFields = byNameDiff(lhsFields, rhsFields)
      val deletedNonPrivateFields = deletedFields filter (m => m.access != "private")
// Deleting a class member or constructor that is not declared private may cause a linkage error if the member or constructor is used by a pre-existing binary.
      err(deletedNonPrivateMethods nonEmpty)("Must not delete non-private methods: "+ deletedNonPrivateMethods mkString) ++ {
  // Deleting a class member or constructor that is not declared private may cause a linkage error if the member or constructor is used by a pre-existing binary.
        err(deletedNonPrivateFields nonEmpty)("Must not delete non-private fields: "+ deletedNonPrivateFields mkString)
      } ++ {
/* TODO:
No incompatibility with pre-existing binaries is caused by
  adding an instance (respectively static)
     member that has the same name, accessibility, (for fields)
       or same name, accessibility, signature, and return type (for methods)
     as an instance (respectively static) member of a superclass or subclass.
*/
        // if the access modifier is changed from default access to private access; from protected access to default or private access; or from public access to protected, default, or private access
        def lessAccess(lm: MemberDisassembly, rm: MemberDisassembly): List[ClassChange] =
          err((  lm.isDefaultAccess && rm.isPrivate
              || lm.isProtected && (rm.isDefaultAccess|| rm.isPrivate)
              || lm.isPublic && (rm.isProtected || rm.isDefaultAccess || rm.isPrivate)))("Must not restrict access of member "+(lm, rm))
        def changesMeta(lm: MemberDisassembly, rm: MemberDisassembly): List[ClassChange] =
          err(!rm.isPrivate && lm.isStatic != rm.isStatic)("Must not change static-ness "+(lm, rm))
        def makesAbstract(lm: MemberDisassembly, rm: MemberDisassembly): List[ClassChange] =
          err(!rm.isAbstract || lm.isAbstract)("Must not make abstract "+(lm, rm))
        def makesFinal(lm: MemberDisassembly, rm: MemberDisassembly): List[ClassChange] =
          err(!lm.isFinal && rm.isFinal)("Must not make final "+(lm, rm))
        val keptMethods = lhsMethods -- deletedMethods
        for(lm <- keptMethods;
            rm <- sameName(rhsMethods, lm)) yield {
          lessAccess(lm, rm) ++ changesMeta(lm, rm) ++ makesAbstract(lm, rm)
        }


      }

    }
  }
}

}
  /*
          val xs1 = lhsClass.methods
          val xs2 = rhsClass.methods

          if (xs1 == xs2)
          else {
            val ldiff = xs1 filterNot (xs2 contains _)
            val rdiff = xs2 filterNot (xs1 contains _)

            if (ldiff ++ rdiff forall (_.name == "readResolve")) rresolve += className
            else {
              val tagged = (ldiff map (x => (x, true))) ++ (rdiff map (x => (x, false)))
              val combined = tagged sortBy (_._1.name) map {
                case (x, true)  => "< " + x.signature
                case (x, false) => "> " + x.signature
              }
              changed += ((className, combined))
            }

   */
