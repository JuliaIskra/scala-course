package calculator

enum Expr:
  case Literal(v: Double)
  case Ref(name: String)
  case Plus(a: Expr, b: Expr)
  case Minus(a: Expr, b: Expr)
  case Times(a: Expr, b: Expr)
  case Divide(a: Expr, b: Expr)

object Calculator extends CalculatorInterface:
 import Expr.*

  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] =
    namedExpressions.map((name, expr) =>
      (name, Signal.Var(eval(expr(), namedExpressions))))

  def eval(expr: Expr, references: Map[String, Signal[Expr]])(using Signal.Caller): Double =
    evalRecur(expr, references, Set())

  private def evalRecur(expr: Expr, references: Map[String, Signal[Expr]], previousReferences: Set[String])(using Signal.Caller): Double =
    expr match {
      case Literal(v) => v
      case Ref(name) =>
        if (previousReferences.contains(name)) {
          Double.NaN
        } else {
          evalRecur(getReferenceExpr(name, references), references, previousReferences + name)
        }
      case Plus(a, b) => evalRecur(a, references, previousReferences) + evalRecur(b, references, previousReferences)
      case Minus(a, b) => evalRecur(a, references, previousReferences) - evalRecur(b, references, previousReferences)
      case Times(a, b) => evalRecur(a, references, previousReferences) * evalRecur(b, references, previousReferences)
      case Divide(a, b) => evalRecur(a, references, previousReferences) / evalRecur(b, references, previousReferences)
    }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]])(using Signal.Caller): Expr =
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
