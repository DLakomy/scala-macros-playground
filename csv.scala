package csv

import scala.quoted.*

trait CsvRowEncoder[A]:
  def header: String
  def asLine(self: A): String


extension [A: CsvRowEncoder as C](self: A)
  inline def asCsvLine = C.asLine(self)
  inline def csvHeader = C.header


private def makeEncoder[A](hdr: String, asLineFn: A => String): CsvRowEncoder[A] =
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
  inline def derived[A]: CsvRowEncoder[A] =
    ${ derivedImpl[A] }

  def derivedImpl[A: Type](using Quotes): Expr[CsvRowEncoder[A]] =
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
                case '{ csvLabel($s) } => s.valueOrAbort

          alias.getOrElse(camelCase2snakeCase(sym.name))
        .mkString(",")
    )

    def makeRowFn(param: Expr[A]): Expr[String] =
      val paramTerm = param.asTerm
      val fields    = cls.caseFields

      fields
        .map: field =>
          val select       = paramTerm.select(field)
          val isString     = select.tpe.widen =:= TypeRepr.of[String]
          val withToString = '{ ${ select.asExpr }.toString }
          if isString then select.asExprOf[String]
          else withToString.asExprOf[String]
        .reduce: (acc, fld) =>
          '{ ${ acc } + "," + ${ fld } }
    end makeRowFn

    '{ makeEncoder($headerExpr, (a: A) => ${ makeRowFn('{ a }) }) }
