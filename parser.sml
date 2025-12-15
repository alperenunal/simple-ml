structure Parser:
sig
  val parse: string -> Ast.program
end =
struct
  structure SimpleMLLrVals =
    SimpleMLLrValsFun (structure Token = LrParser.Token)
  structure SimpleMLLex =
    SimpleMLLexFun (structure Tokens = SimpleMLLrVals.Tokens)
  structure SimpleMLParser =
    Join
      (structure LrParser = LrParser
       structure ParserData = SimpleMLLrVals.ParserData
       structure Lex = SimpleMLLex)

  fun invoke lexstream =
    let
      fun print_error (s, i: int, _) =
        TextIO.output
          (TextIO.stdOut, "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")
    in
      SimpleMLParser.parse (0, lexstream, print_error, ())
    end

  fun parse f =
    let
      val file = TextIO.openIn f
      fun reader _ = TextIO.input file
      val lexer = SimpleMLParser.makeLexer reader
      val (result, _) = invoke lexer
    in
      result
    end
end
