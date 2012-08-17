package org.deca.compiler

import org.deca.compiler.parser.parser._
import org.deca.compiler.parser.lexer._
import org.deca.compiler.parser.node._
import org.deca.compiler.definition._
import org.deca.compiler.expression._
import org.deca.compiler.signature._
import java.io.FileReader
import java.io.PushbackReader

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
    val mod = ASTProcessor.processModuleDefinition(new AModuledefDefinition(module),GlobalScope)
    val path = file.split('/').init.mkString
    mod.setPath(if(path != "") path + "/" else path)
    mod
  }
  
  def main(args: Array[String]): Unit = {
    //We reference the singletons for the primitive types to get their classes loaded and their names declared.
    Byte;
    Octet;
    SInt;
    SNat;
    Int;
    Nat;
    LongInt;
    LongNat;
    LongInteger;
    BuiltInSums;
    FP128Type;
    DoubleType;
    FloatType;
    if(args.length == 0 || args(0) == "--help" || args(0) == "--version")
      System.err.println("Deca compiler version 0.2.1")
    if(args.length == 0 || args(0) == "--help") {
      System.err.println("Possible arguments: ")
      System.err.println("  --help => display this screen")
      System.err.println("  --version => show the compiler version")
      System.err.println("  _ => compile the given Deca source files")
    }
    else try {
      val modules = args.map(arg => compile(arg))
      for(module <- modules)
        module.writeBitcode
    }
    catch {
      case init: ExceptionInInitializerError => throw init.getCause
    }
  }
}
