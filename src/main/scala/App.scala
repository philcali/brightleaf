package brightleaf

import collection.immutable.PagedSeq
import io.Source
import util.parsing.combinator.RegexParsers
import util.parsing.input.PagedSeqReader

object App extends scala.App {
}

// TODO: in great need to breaking this up...
// TODO: About to switch gears...
// parser dependencies can be easily provided via cake
trait BrightLeafParser extends RegexParsers {

  lazy val types = List(
    "string",
    "double",
    "integer",
    "float",
    "object",
    "boolean",
    "function",
    "interface",
    "invalid",
    "dynamic",
    "void"
  )

  lazy val binary = List(
    "=".r,
    "<>".r,
    "<".r,
    ">".r,
    ">=".r,
    "<=".r,
    """\+""".r,
    """\*""".r,
    "/".r,
    """\-""".r,
    "(?i)mod".r,
    "(?i)and".r,
    "(?i)or".r,
    """\^""".r
  )

  lazy val unary = List(
    """\+""".r,
    """\-""".r,
    "(?i)not".r
  )

  lazy val keywords = List(
    "function",
    "dim",
    "end",
    "sub",
    "while",
    "stop",
    "to",
    "if",
    "else",
    "then",
    "exit",
    "print",
    "rem",
    "goto",
    "as",
    "return",
    "next",
    "pos",
    "for",
    "in",
    "elseif",
    "endif",
    "endwhile",
    "endfor",
    "endfunction",
    "endsub",
    "eval",
    "each",
    "false",
    "true",
    "step",
    "run",
    "let",
    "invalid"
  )

  lazy val keywordsRegex = Map(keywords.map(caseInsensitive):_*)
  lazy val typesRegex = Map(types.map(caseInsensitive):_*)
  lazy val anyType = typesRegex.values.map(regex).reduceLeft(_ | _)
  lazy val identifier = """[a-zA-Z_]+""".r ^? ({
    case name if !keywordsRegex.contains(name.toLowerCase) => name
  })
  lazy val variable = identifier <~ "[$%!#]?".r
  lazy val booleanLiteral = (keywordsRegex("true") | keywordsRegex("false"))
  lazy val integerLiteral = """\d+%?""".r
  lazy val floatLiteral = """\d+(:?\.\d+)?!?""".r
  lazy val doubleLiteral = """\d+(:?\.\d+)?#?""".r
  lazy val stringLiteral = "\"([^\"]+)\"".r
  lazy val anyLiteral = booleanLiteral | integerLiteral | floatLiteral | doubleLiteral | stringLiteral
  lazy val value = keywordsRegex("invalid") | anyLiteral | complexAccess
  lazy val unaryOps = (unary.map(regex).reduceLeft(_ | _)) ~ value
  lazy val binaryOps = value ~ (binary.map(regex).reduceLeft(_ | _)) ~ value
  lazy val parens = "(" ~> binaryOps | unaryOps <~ ")"
  lazy val funcArg = variable ~ opt(typeDef)
  lazy val arguments = "(" ~> repsep(funcArg, ",") <~ ")"
  lazy val typeDef = keywordsRegex("as") ~> anyType
  lazy val expression = binaryOps | unaryOps | parens | value | arrayLiteral | objectLiteral
  lazy val assignment = complexAccess ~ "=" ~ expression
  lazy val complexAccess: Parser[Any] = variable ~ rep(functionInvoke | arrayAccess | dotNotation)
  lazy val functionInvoke = "(" ~> repsep(expression, ",") <~ ")"
  lazy val arrayAccess = "[" ~> expression <~ "]"
  lazy val dotNotation = "." ~> identifier
  lazy val arrayLiteral: Parser[Seq[Any]] = "[" ~> repsep(expression, ",") <~ "]"
  lazy val objectMember = identifier ~ ":" ~ expression
  lazy val objectLiteral: Parser[Seq[Any]] = "{" ~> repsep(objectMember, ",") <~ "}"
  lazy val comment = (keywordsRegex("rem") | "`".r) ~> """([^\n])+""".r
  lazy val globalFunction = keywordsRegex("function") ~> identifier ~ arguments ~ opt(typeDef) ~ statements <~ endStatement("function")
  lazy val function = keywordsRegex("function") ~> arguments ~ opt(typeDef) ~ statements <~ endStatement("function")
  lazy val label = identifier <~ ":" ~ "\n"
  lazy val goto = keywordsRegex("goto") ~> identifier
  lazy val ifSingle = keywordsRegex("if") ~> expression ~ singleConditional ~ opt(keywordsRegex("else") ~> statement)
  lazy val ifMulit = keywordsRegex("if") ~> expression ~ conditionalBody ~ opt(keywordsRegex("elseif") ~> expression ~ conditionalBody) ~ opt(keywordsRegex("else") ~ statements) <~ endStatement("if")
  lazy val singleConditional: Parser[Any] = opt(keywordsRegex("then")) ~> statement
  lazy val conditionalBody: Parser[Seq[Any]] = opt(keywordsRegex("then")) ~ """\n""".r ~> statements
  lazy val foreachStatement = keywordsRegex("for") ~ keywordsRegex("each") ~> identifier ~ keywordsRegex("in") ~ complexAccess ~ statements <~ endStatement("for")
  lazy val forCounter = variable ~ "=" ~ expression
  lazy val forStatement = keywordsRegex("for") ~> forCounter ~ keywordsRegex("to") ~ expression ~ opt(stepExpr) ~ statements <~ endStatement("for")
  lazy val stepExpr = keywordsRegex("step") ~> expression
  lazy val whileStatement = keywordsRegex("while") ~> expression ~ statements <~ endStatement("while")
  lazy val dimStatement = keywordsRegex("dim") ~> identifier ~ dimArguments
  lazy val dimArguments = "[" ~> repsep(integerLiteral, "," | "][") <~ "]"
  lazy val returnStatement = keywordsRegex("return") ~> expression
  lazy val printStatement = keywordsRegex("print") ~> repsep(expression, "," | ";")
  lazy val statement: Parser[Any] = (
      ifMulit
    | ifSingle
    | printStatement
    | returnStatement
    | dimStatement
    | foreachStatement
    | forStatement
    | whileStatement
    | function
    | globalFunction
    | comment
    | assignment
    | complexAccess
  )
  lazy val statements: Parser[Seq[Any]] = repsep(statement, ":" | """\n*""".r)
  lazy val program = phrase(statements)
  def apply(in: String) = parseAll(program, in)
  def apply(source: Source) = parseAll(program, new PagedSeqReader(PagedSeq.fromSource(source)))
  def endStatement(state: String) = (keywordsRegex("end") ~ keywordsRegex(state)) | keywordsRegex(s"end${state}")
  def caseInsensitive(str: String) = str -> s"(?i)${str}".r
}
