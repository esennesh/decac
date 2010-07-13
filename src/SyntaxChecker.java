import java.io.FileReader;
import java.io.PushbackReader;
import java.io.FileNotFoundException;
import java.io.IOException;
import decasyntax.parser.*;
import decasyntax.lexer.*;
import decasyntax.node.*;

public class SyntaxChecker {
	public static Start check_syntax(String file) throws IOException,FileNotFoundException,ParserException,LexerException {
		Parser parser = new Parser(new Lexer(new PushbackReader(new FileReader(file))));
		return parser.parse();
	}

	public static void main(String[] args) throws IOException,FileNotFoundException,ParserException,LexerException {
		System.out.println("Compiling " + args[0] + ".");
		try {
			Start node = check_syntax(args[0]);
		}
		catch(ParserException pe) {
			System.err.println(pe.getMessage() + " found '" + pe.getToken().getText() + "' token of " + pe.getToken().getClass().getName() + " type.");
		}
	}
}
