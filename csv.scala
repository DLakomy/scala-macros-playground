package csv

import scala.quoted.*

trait CsvRowEncoder[A <: Product]:
  def header: String
  def asLine(self: A): String


extension [A <: Product: CsvRowEncoder as C](self: A)
  inline def asCsvLine = C.asLine(self)
  inline def csvHeader = C.header


private def makeEncoder[A <: Product](hdr: String, asLineFn: A => String): CsvRowEncoder[A] =
  new CsvRowEncoder:
    override def header: String          = hdr
    override def asLine(self: A): String = asLineFn(self)


private inline def camelCase2snakeCase(inline camelCaseString: String): String =
  camelCaseString.foldLeft(""): (acc, char) =>
    if (char.isUpper)
      if (acc.isEmpty) char.toLower.toString
      else acc + "_" + char.toLower
    else
      acc + char


object CsvRowEncoder:
  inline def derived[A <: Product]: CsvRowEncoder[A] =
    ${ derivedImpl[A] }

  def derivedImpl[A <: Product: Type](using Quotes): Expr[CsvRowEncoder[A]] =
    import quotes.reflect.*

    val cls = TypeRepr.of[A].classSymbol.getOrElse(throw RuntimeException("A class is expected"))

    if !cls.flags.is(Flags.Case) then
      report.errorAndAbort("CsvRowEncoder can be derived only for a case class")

    val headerExpr = Expr(
      cls.primaryConstructor.paramSymss.flatten
        .map: sym =>
          val alias = sym.annotations
            .find(_.tpe =:= TypeRepr.of[csvLabel])
            .map: annTerm =>
              annTerm.asExpr match
                case '{ csvLabel($s) } => s.value.get

          alias.getOrElse(camelCase2snakeCase(sym.name))
        .mkString(",")
    )

    def makeRowFn(param: Expr[A]): Expr[String] =
      val paramTerm = param.asTerm
      val fields    = TypeRepr.of[A].classSymbol.get.caseFields

      fields
        .map: field =>
          val select       = paramTerm.select(field)
          val isBoolean    = Expr(select.tpe.widen =:= TypeRepr.of[Boolean])
          val withToString = '{ ${ paramTerm.select(field).asExpr }.toString }
          isBoolean match
            case Expr(true)  => withToString.asExprOf[String]
            case Expr(false) => select.asExprOf[String]
        .reduce: (acc, fld) =>
          '{ ${ acc } + "," + ${ fld } }
    end makeRowFn

    '{ makeEncoder($headerExpr, (a: A) => ${ makeRowFn('{ a }) }) }
