package decac

import decasyntax.parser._
import decasyntax.lexer._
import decasyntax.node._
import java.io.FileReader
import java.io.PushbackReader

object Decac {
  System.loadLibrary("jllvm")

  def check_syntax(file: String): Start = {
    System.out.println("Parsing " + file + ".")
    try {
      val parser: Parser = new Parser(new Lexer(new PushbackReader(new FileReader(file))))
      return parser.parse()
    }
    catch {
      case pe: ParserException => {
        System.err.println(pe.getMessage() + " found '" + pe.getToken().getText() + "' token of " + pe.getToken().getClass().getName() + " type.")
        throw pe
      }
    }
  }
  
  def compile(file: String): Unit = {
    val module: PModuleDefinition = check_syntax(file).getPModuleDefinition()
    val definition = ASTProcessor.processDefinition(new AModuledefDefinition(module),GlobalScope)
  }
  
  def main(args: Array[String]): Unit = {
    val modules = args.map(arg => compile(arg))
    modules.foreach(mod => mod match { case module: Module => module.compiledModule })
    System.err.println("Beginning AST print-out for debugging purposes.")
    for(symbol <- GlobalScope.symbols)
      System.err.println(symbol.toString)
    System.err.println("Ending AST print-out for debugging purposes.")
  }
}
