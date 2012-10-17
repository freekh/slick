package scala.slick.compiler

import scala.slick.ast._
import Util.nodeToNodeOps

/** Compute type information for all nodes in the AST */
class AssignTypes extends Phase {
  val name = "assignTypes"

  class LoggingSymbolScope(val m: Map[Symbol, Type]) extends SymbolScope {
    def + (entry: (Symbol, Type)) = new LoggingSymbolScope(m + entry)
    def get(sym: Symbol): Option[Type] = m.get(sym)
    def computed(n: Node, tpe: Type): Unit =
      logger.debug("Assigned type "+tpe+" to node "+n)
    def withDefault(f: (Symbol => Type)) = new LoggingSymbolScope(m.withDefault(f))
  }

  def apply(tree: Node, state: CompilationState): Node = {
    tree.nodeGetType(new LoggingSymbolScope(Map.empty))
    tree
  }
}
