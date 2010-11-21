package decac

import decasyntax.parser._
import decasyntax.lexer._
import decasyntax.node._
import java.io.FileReader
import java.io.PushbackReader
import jllvm.LLVMBitWriter

object Decac {
  System.loadLibrary("jllvm")

  def check_syntax(file: String): Start = {
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
  
  def compile(file: String): Module = {
    val module: PModuleDefinition = check_syntax(file).getPModuleDefinition()
    ASTProcessor.processDefinition(new AModuledefDefinition(module),GlobalScope) match {
      case mod: Module => mod
      case _ => throw new Exception("Processing a module definition must yield a Module.")
    }
  }
  
  def main(args: Array[String]): Unit = {
    //We reference the singletons for the primitive types to get their classes loaded and their names declared.
    Byte;
    Octet;
    BooleanGamma;
    FloatGamma;
    val modules = args.map(arg => compile(arg))
    for(module <- modules)
      (new LLVMBitWriter(module.compile)).writeBitcodeToFile(module.name + ".llas")
  }
}
