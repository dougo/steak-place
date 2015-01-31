// $ANTLR : "Structure.g" -> "StructureParser.java"$

import edu.neu.ccs.demeter.*;

import antlr.TokenBuffer;
import antlr.TokenStreamException;
import antlr.TokenStreamIOException;
import antlr.ANTLRException;
import antlr.LLkParser;
import antlr.Token;
import antlr.TokenStream;
import antlr.RecognitionException;
import antlr.NoViableAltException;
import antlr.MismatchedTokenException;
import antlr.SemanticException;
import antlr.ParserSharedInputState;
import antlr.collections.impl.BitSet;

public class StructureParser extends antlr.LLkParser       implements StructureParserTokenTypes
 {

  static char unescapifyChar(String s) {
    char c = s.charAt(0);
    if (c == '\\') {
      switch (s.charAt(1)) {
      case 'n': c = '\n'; break;
      case 't': c = '\t'; break;
      case 'b': c = '\b'; break;
      case 'r': c = '\r'; break;
      case 'f': c = '\f'; break;
      case '\\': c = '\\'; break;
      case '\'': c = '\''; break;
      case '\"': c = '\"'; break;
      default:
       c = (char) Integer.parseInt(s.substring(1, s.length()), 8);
       break;
      }
    }
    return c;
  }
  static String unescapify(String s) {
    char str[] = new char[s.length()];
    int i = 0, o = 0;
    while (i < s.length()) {
      char c = s.charAt(i++);
      if (c == '\\') {
       int j = i + 1;
       while (j < s.length() &&
              Character.digit(s.charAt(j), 8) != -1) {
         j++;
       }
       c = unescapifyChar(s.substring(i-1, j));
       i = j;
      }
      str[o++] = c;
    }
    return String.valueOf(str, 0, o);
  }

protected StructureParser(TokenBuffer tokenBuf, int k) {
  super(tokenBuf,k);
  tokenNames = _tokenNames;
}

public StructureParser(TokenBuffer tokenBuf) {
  this(tokenBuf,1);
}

protected StructureParser(TokenStream lexer, int k) {
  super(lexer,k);
  tokenNames = _tokenNames;
}

public StructureParser(TokenStream lexer) {
  this(lexer,1);
}

public StructureParser(ParserSharedInputState state) {
  super(state,1);
  tokenNames = _tokenNames;
}

	public final A  parseA() throws RecognitionException, TokenStreamException {
		A it;
		
		
		it = new A();
		B the_b = null;
		C the_c = null;
		D the_d = null;
		
		
		match(LITERAL_a);
		the_b=parseB();
		it.b = the_b;
		the_c=parseC();
		it.c = the_c;
		the_d=parseD();
		it.d = the_d;
		commonY(it);
		return it;
	}
	
	public final B  parseB() throws RecognitionException, TokenStreamException {
		B it;
		
		
		it = new B();
		Z the_z = null;
		
		
		match(LITERAL_b);
		the_z=parseZ();
		it.z = the_z;
		commonY(it);
		return it;
	}
	
	public final C  parseC() throws RecognitionException, TokenStreamException {
		C it;
		
		
		it = new C();
		E the_e = null;
		
		
		the_e=parseE();
		it.e = the_e;
		return it;
	}
	
	public final D  parseD() throws RecognitionException, TokenStreamException {
		D it;
		
		
		it = new D();
		Y the_y = null;
		
		
		match(LITERAL_d);
		the_y=parseY();
		it.y = the_y;
		commonZ(it);
		return it;
	}
	
	public final void commonY(
		Y it
	) throws RecognitionException, TokenStreamException {
		
		
		
		
	}
	
	public final Z  parseZ() throws RecognitionException, TokenStreamException {
		Z it;
		
		
		switch ( LA(1)) {
		case LITERAL_d:
		{
			it=parseD();
			break;
		}
		case LITERAL_e:
		{
			it=parseE();
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		return it;
	}
	
	public final Y  parseY() throws RecognitionException, TokenStreamException {
		Y it;
		
		
		switch ( LA(1)) {
		case LITERAL_a:
		{
			it=parseA();
			break;
		}
		case LITERAL_b:
		{
			it=parseB();
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		return it;
	}
	
	public final void commonZ(
		Z it
	) throws RecognitionException, TokenStreamException {
		
		
		
		
	}
	
	public final E  parseE() throws RecognitionException, TokenStreamException {
		E it;
		
		
		it = new E();
		
		
		match(LITERAL_e);
		commonZ(it);
		return it;
	}
	
	public final boolean  parseboolean() throws RecognitionException, TokenStreamException {
		boolean it;
		
		
		switch ( LA(1)) {
		case LITERAL_true:
		{
			match(LITERAL_true);
			it = true;
			break;
		}
		case LITERAL_false:
		{
			match(LITERAL_false);
			it = false;
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		return it;
	}
	
	public final char  parsechar() throws RecognitionException, TokenStreamException {
		char it;
		
		Token  t = null;
		
		t = LT(1);
		match(CHAR_LITERAL);
		String s = t.getText();
		it = unescapifyChar(s.substring(1, s.length()-1));
		return it;
	}
	
	public final byte  parsebyte() throws RecognitionException, TokenStreamException {
		byte it;
		
		int i;
		
		i=parseint();
		it = (byte) i;
		return it;
	}
	
	public final int  parseint() throws RecognitionException, TokenStreamException {
		int it;
		
		Token  t = null;
		
		t = LT(1);
		match(NUM_INT);
		it = Integer.parseInt(t.getText());
		return it;
	}
	
	public final short  parseshort() throws RecognitionException, TokenStreamException {
		short it;
		
		int i;
		
		i=parseint();
		it = (short) i;
		return it;
	}
	
	public final long  parselong() throws RecognitionException, TokenStreamException {
		long it;
		
		Token  t = null;
		
		t = LT(1);
		match(NUM_LONG);
		String s = t.getText();
		it = Long.parseLong(s.substring(0,s.length()-1));
		return it;
	}
	
	public final float  parsefloat() throws RecognitionException, TokenStreamException {
		float it;
		
		Token  t = null;
		
		t = LT(1);
		match(NUM_FLOAT);
		String s = t.getText();
		it = Float.parseFloat(s.substring(0,s.length()-1));
		return it;
	}
	
	public final double  parsedouble() throws RecognitionException, TokenStreamException {
		double it;
		
		Token  t = null;
		
		t = LT(1);
		match(NUM_DOUBLE);
		String s = t.getText();
		if (s.endsWith("D") || s.endsWith("d"))
		it = Double.parseDouble(s.substring(0,s.length()-1));
		else
		it = Double.parseDouble(s);
		return it;
	}
	
	public final Boolean  parseBoolean() throws RecognitionException, TokenStreamException {
		Boolean it;
		
		
		switch ( LA(1)) {
		case LITERAL_true:
		{
			match(LITERAL_true);
			it = Boolean.TRUE;
			break;
		}
		case LITERAL_false:
		{
			match(LITERAL_false);
			it = Boolean.FALSE;
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		return it;
	}
	
	public final Character  parseCharacter() throws RecognitionException, TokenStreamException {
		Character it;
		
		char c;
		
		c=parsechar();
		it = new Character(c);
		return it;
	}
	
	public final Byte  parseByte() throws RecognitionException, TokenStreamException {
		Byte it;
		
		byte b;
		
		b=parsebyte();
		it = new Byte(b);
		return it;
	}
	
	public final Integer  parseInteger() throws RecognitionException, TokenStreamException {
		Integer it;
		
		int i;
		
		i=parseint();
		it = new Integer(i);
		return it;
	}
	
	public final Long  parseLong() throws RecognitionException, TokenStreamException {
		Long it;
		
		long l;
		
		l=parselong();
		it = new Long(l);
		return it;
	}
	
	public final Float  parseFloat() throws RecognitionException, TokenStreamException {
		Float it;
		
		float f;
		
		f=parsefloat();
		it = new Float(f);
		return it;
	}
	
	public final Double  parseDouble() throws RecognitionException, TokenStreamException {
		Double it;
		
		double d;
		
		d=parsedouble();
		it = new Double(d);
		return it;
	}
	
	public final Number  parseNumber() throws RecognitionException, TokenStreamException {
		Number it;
		
		
		switch ( LA(1)) {
		case NUM_INT:
		{
			it=parseInteger();
			break;
		}
		case NUM_LONG:
		{
			it=parseLong();
			break;
		}
		case NUM_FLOAT:
		{
			it=parseFloat();
			break;
		}
		case NUM_DOUBLE:
		{
			it=parseDouble();
			break;
		}
		default:
		{
			throw new NoViableAltException(LT(1), getFilename());
		}
		}
		return it;
	}
	
	public final String  parseString() throws RecognitionException, TokenStreamException {
		String it;
		
		Token  t = null;
		
		t = LT(1);
		match(STRING_LITERAL);
		String s = t.getText();
		it = unescapify(s.substring(1, s.length()-1));
		return it;
	}
	
	public final Ident  parseIdent() throws RecognitionException, TokenStreamException {
		Ident it;
		
		Token  t = null;
		
		t = LT(1);
		match(IDENT);
		it = new Ident(t.getText());
		return it;
	}
	
	
	public static final String[] _tokenNames = {
		"<0>",
		"EOF",
		"<2>",
		"NULL_TREE_LOOKAHEAD",
		"\"a\"",
		"\"b\"",
		"\"d\"",
		"\"e\"",
		"\"true\"",
		"\"false\"",
		"CHAR_LITERAL",
		"NUM_INT",
		"NUM_LONG",
		"NUM_FLOAT",
		"NUM_DOUBLE",
		"STRING_LITERAL",
		"IDENT",
		"DOT",
		"WS",
		"SL_COMMENT",
		"ML_COMMENT",
		"ESC",
		"HEX_DIGIT",
		"VOCAB",
		"EXPONENT",
		"FLOAT_SUFFIX"
	};
	
	
	}
