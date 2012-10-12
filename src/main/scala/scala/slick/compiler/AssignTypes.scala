package scala.slick.compiler

import scala.slick.ast._
import Util.nodeToNodeOps

/** Compute type information for all nodes in the AST */
class AssignTypes extends Phase {
  val name = "assignTypes"

  def apply(tree: Node, state: CompilationState): Node = {
    def tr(n: Node, scope: Scope, f: (Node, Scope) => Node): Node = {
      val n2 = n.mapChildrenWithScope({ (_, ch, chscope) => tr(ch, chscope, f) }, scope)
      f(n2, scope)
    }
    tr(tree, Scope.empty, { (n, sc) =>
      n.nodeGetType(sc)
      logger.debug("Assigned type "+n.nodeCurrentType+" to "+n)
      n
    })
  }
}