package brightleaf

import util.parsing.combinator.RegexParsers

object App extends scala.App {
}

trait BrightLeafTokenizer extends RegexParsers {
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

  lazy val comparisons = List(
    "=".r,
    "<>".r,
    "<".r,
    ">".r,
    ">=".r,
    "<=".r
  )

  lazy val binary = List(
    """\+""".r,
    """\*""".r,
    "/".r,
    """\-""".r,
    "?imod".r,
    "?iand".r,
    "?ior".r,
    """\^""".r
  )

  lazy val unary = List(
    """\+""".r,
    """\-""".r,
    "?inot".r
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
    "type",
    "next",
    "not",
    "and",
    "or",
    "pos",
    "CreateObject",
    "elseif",
    "endif",
    "endwhile",
    "endfor",
    "endfunction",
    "endsub",
    "box",
    "eval",
    "false",
    "true",
    "step",
    "run",
    "let",
    "mod",
    "invalid",
    "line_num",
    "GetGlobalAA",
    "GetLastRunCompileError",
    "GetLastRunRunTimeError",
    "objfun",
    "tab"
  )
  lazy val keywordsRegex = Map(keywords.map(caseInsensitive):_*)
  lazy val typesRegex = Map(types.map(caseInsensitive):_*)
  lazy val anyType = typesRegex.values.map(regex).reduceLeft(_ | _)
  lazy val identifier = """[a-zA-Z_]+"""
  lazy val variable = identifier + "[$%!#]?".r
  lazy val booleanLiteral = (keywordsRegex("true") | keywordsRegex("false"))
  lazy val integerLiteral = """\d+%?""".r
  lazy val floatLiteral = """\d+(:?\.\d+)?!?""".r
  lazy val doubleLiteral = """\d+(:?\.\d+)?#?""".r
  lazy val stringLiteral = "\"([^\"]+)\"".r
  lazy val anyLiteral = booleanLiteral | integerLiteral | floatLiteral | doubleLiteral | stringLiteral
  lazy val value = anyLiteral | complexAccess
  lazy val unaryOps = (unary.map(regex).reduceLeft(_ | _)) ~ value
  lazy val binaryOps = value ~ (binary.map(regex).reduceLeft(_ | _)) ~ value
  lazy val parens = "(" ~> binaryOps | unaryOps <~ ")"
  lazy val funcArg = variable ~ opt(typeDef)
  lazy val arguments = "(" ~> repsep(funcArg, ",") <~ ")"
  lazy val typeDef = keywordsRegex("as") ~> anyType
  lazy val expression = value | parens | unaryOps | binaryOps
  lazy val reference = keywordsRegex("invalid") | expression | arrayLiteral | objectLiteral
  lazy val assignment = complexAccess ~ "=" ~ reference
  lazy val complexAccess: Parser[Any] = variable ~ rep(functionInvoke | arrayAccess | dotNotation)
  lazy val functionInvoke = "(" ~> repsep(reference, ",") <~ ")"
  lazy val arrayAccess = "[" ~> expression <~ "]"
  lazy val dotNotation = "." ~> identifier
  lazy val arrayLiteral: Parser[Seq[Any]] = "[" ~> repsep(reference, ",") <~ "]"
  lazy val objectMember = identifier ~ ":" ~ reference
  lazy val objectLiteral: Parser[Seq[Any]] = "{" ~> repsep(objectMember, ",") <~ "}"
  lazy val comment = (keywordsRegex("rem") | "`".r) ~> """([^\n])+""".r
  lazy val globalFunction = keywordsRegex("function") ~> identifier ~ arguments ~ opt(typeDef) ~ statements <~ endStatement("function")
  lazy val function = keywordsRegex("function") ~> arguments ~ opt(typeDef) ~ statements <~ endStatement("function")
  lazy val label = identifier <~ ":" ~ "\n"
  lazy val goto = keywordsRegex("goto") ~> identifier
  lazy val ifSingle = keywordsRegex("if") ~> expression ~ conditionalBody ~ opt(keywordsRegex("else") ~> statements)
  lazy val ifMulit = keywordsRegex("if") ~> expression ~ conditionalBody ~ rep(keywordsRegex("elseif") ~> conditionalBody) ~ opt(keywordsRegex("else") ~> statements) <~ endStatement("if")
  lazy val conditionalBody: Parser[Seq[Any]] = opt(keywordsRegex("then")) ~> statements
  lazy val foreachStatement = keywordsRegex("for") ~ keywordsRegex("each") ~> identifier ~ keywordsRegex("in") ~ complexAccess ~ statements <~ endStatement("for")
  lazy val forCounter = variable ~ "=" ~ expression
  lazy val forStatement = keywordsRegex("for") ~> forCounter ~ keywordsRegex("to") ~ expression ~ opt(stepExpr) ~ statements <~ endStatement("for")
  lazy val stepExpr = keywordsRegex("step") ~> expression
  lazy val whileStatement = keywordsRegex("while") ~> expression ~ statements <~ endStatement("while")
  lazy val statement = ifSingle | ifMulit | foreachStatement | forStatement | whileStatement | assignment | comment | function | globalFunction
  lazy val statements: Parser[Seq[Any]] = repsep(statement, ":" | "\n")
  lazy val program = phrase(statements)
  def endStatement(state: String) = (keywordsRegex("end") ~ keywordsRegex(state)) | keywordsRegex("end" + state)
  def caseInsensitive(str: String) = str -> s"?i${str}".r
}
