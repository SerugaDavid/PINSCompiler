/**
 * @Author: turk
 * @Description: Sintaksni analizator.
 */

package compiler.parser;

import static compiler.lexer.TokenType.*;
import static common.RequireNonNull.requireNonNull;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import common.Report;
import compiler.lexer.Position;
import compiler.lexer.Symbol;
import compiler.parser.ast.Ast;
import compiler.parser.ast.def.*;
import compiler.parser.ast.expr.Binary;
import compiler.parser.ast.expr.Expr;
import compiler.parser.ast.expr.Where;
import compiler.parser.ast.type.Array;
import compiler.parser.ast.type.Atom;
import compiler.parser.ast.type.Type;
import compiler.parser.ast.type.TypeName;

public class Parser {
    /**
     * Seznam leksikalnih simbolov.
     */
    private final List<Symbol> symbols;
    private int index;

    /**
     * Ciljni tok, kamor izpisujemo produkcije. Če produkcij ne želimo izpisovati,
     * vrednost opcijske spremenljivke nastavimo na Optional.empty().
     */
    private final Optional<PrintStream> productionsOutputStream;

    public Parser(List<Symbol> symbols, Optional<PrintStream> productionsOutputStream) {
        requireNonNull(symbols, productionsOutputStream);
        this.symbols = symbols;
        this.productionsOutputStream = productionsOutputStream;
        this.index = 0;
    }

    /**
     * Izvedi sintaksno analizo.
     */
    public Ast parse() {
        Defs ast = parseSource();
        return ast;
    }

    private Defs parseSource() {
        dump("source -> definitions");
        Defs defs = parseDefinitions();
        if (getSymbol().tokenType != EOF)
            Report.error("Source\n" + "Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected:\nEOF\n';'\n'}'");
        return defs;
    }

    private Defs parseDefinitions() {
        dump("definitions -> definition definitions2");
        Def def = parseDefinition();
        Defs left = new Defs(def.position, List.of(def));
        return parseDefinitions2(left);
    }

    private Defs parseDefinitions2(Defs left) {
        if (getSymbol().tokenType == OP_SEMICOLON) {
            dump("definitions2 -> ; definition definitions2");
            skipSymbol();
            Def right = parseDefinition();
            ArrayList defs = new ArrayList(left.definitions);
            defs.add(right);
            Defs maybeLeft = new Defs(
                    new Position(
                            left.position.start,
                            right.position.end
                    ),
                    defs.stream().toList()
            );
            return parseDefinitions2(maybeLeft);
        } else {
            dump("definitions2 -> e");
            return left;
        }
    }

    private Def parseDefinition() {
        Position start = getSymbol().position;
        switch (getSymbol().tokenType) {
            case KW_TYP -> {
                dump("definition -> type_definition");
                skipSymbol();
                return parseTypeDefinition(start);
            }
            case KW_FUN -> {
                dump("definition -> function_definition");
                skipSymbol();
                return parseFunctionDefinition(start);
            }
            case KW_VAR -> {
                dump("definition -> variable_definition");
                skipSymbol();
                return parseVariableDefinition(start);
            }
            default -> {
                Report.error("VariableDefinition\n" + "Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected:\nTYP\nFUN\nVAR");
                return null;
            }
        }
    }

    private TypeDef parseTypeDefinition(Position start) {
        dump("type_definition -> typ identifier : type");
        if (getSymbol().tokenType != IDENTIFIER)
            Report.error("TypeDefinition\n" + "Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: IDENTIFIER");
        String name = getSymbol().lexeme;
        skipSymbol();
        if (getSymbol().tokenType != OP_COLON)
            Report.error("TypeDefinition\n" + "Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: ':'");
        skipSymbol();
        Type type = parseType();
        return new TypeDef(
                new Position(
                        start.start,
                        type.position.end
                ),
                name,
                type
        );
    }

    private Type parseType() {
        Symbol current = getSymbol();
        switch (getSymbol().tokenType) {
            case IDENTIFIER:
                dump("type -> identifier");
                skipSymbol();
                return new TypeName(current.position, current.lexeme);
            case AT_LOGICAL:
                dump("type -> logical");
                skipSymbol();
                return Atom.LOG(current.position);
            case AT_INTEGER:
                dump("type -> integer");
                skipSymbol();
                return Atom.INT(current.position);
            case AT_STRING:
                dump("type -> string");
                skipSymbol();
                return Atom.STR(current.position);
            case KW_ARR:
                dump("type -> arr [ int_const ] type");
                skipSymbol();
                if (getSymbol().tokenType != OP_LBRACKET)
                    Report.error("Type\n" + "Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: '['");
                skipSymbol();
                if (getSymbol().tokenType != C_INTEGER)
                    Report.error("Type\n" + "Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: C_INTEGER");
                int size = Integer.parseInt(getSymbol().lexeme);
                skipSymbol();
                if (getSymbol().tokenType != OP_RBRACKET)
                    Report.error("Type\n" + "Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: ']'");
                skipSymbol();
                Type right = parseType();
                return new Array(
                        new Position(
                                current.position.start,
                                right.position.end
                        ),
                        size,
                        right
                );
            default:
                Report.error("Type\n" + "Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected:\nIDENTIFIER\nAT_LOGICAL\nAT_INTEGER\nAT_STRING\nKW_ARR");
        }
        return null;
    }

    private FunDef parseFunctionDefinition(Position start) {
        dump("function_definition -> fun identifier ( parameters ) : type = expression");
        if (getSymbol().tokenType != IDENTIFIER)
            Report.error("FunctionDefinition\n" + "Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: IDENTIFIER");
        String name = getSymbol().lexeme;
        skipSymbol();
        if (getSymbol().tokenType != OP_LPARENT)
            Report.error("FunctionDefinition\n" + "Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: '('");
        skipSymbol();
        List<FunDef.Parameter> parameters = parseParameters();
        if (getSymbol().tokenType != OP_RPARENT)
            Report.error("FunctionDefinition\n" + "Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: ')'");
        skipSymbol();
        if (getSymbol().tokenType != OP_COLON)
            Report.error("FunctionDefinition\n" + "Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: ':'");
        skipSymbol();
        Type type = parseType();
        if (getSymbol().tokenType != OP_ASSIGN)
            Report.error("FunctionDefinition\n" + "Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: '='");
        skipSymbol();
        Expr expr = parseExpression();
        return new FunDef(
                new Position(
                        start.start,
                        expr.position.end
                ),
                name,
                parameters,
                type,
                expr
        );
    }

    private List<FunDef.Parameter> parseParameters() {
        dump("parameters -> parameter parameters2");
        FunDef.Parameter left = parseParameter();
        ArrayList<FunDef.Parameter> parameters = new ArrayList<>();
        parameters.add(left);
        parameters.addAll(parseParameters2());
        return parameters.stream().toList();
    }

    private List<FunDef.Parameter> parseParameters2() {
        if (getSymbol().tokenType == OP_COMMA) {
            skipSymbol();
            dump("parameters2 -> , parameter parameters2");
            FunDef.Parameter right = parseParameter();
            ArrayList<FunDef.Parameter> parameters = new ArrayList<>();
            parameters.add(right);
            parameters.addAll(parseParameters2());
            return parameters.stream().toList();
        } else
            dump("parameters2 -> e");
        return new ArrayList<FunDef.Parameter>().stream().toList();
    }

    private FunDef.Parameter parseParameter() {
        dump("parameter -> identifier : type");
        if (getSymbol().tokenType != IDENTIFIER)
            Report.error("Parameter\n" + "Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: IDENTIFIER");
        Symbol name = getSymbol();
        skipSymbol();
        if (getSymbol().tokenType != OP_COLON)
            Report.error("Parameter\n" + "Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: ':'");
        skipSymbol();
        Type type = parseType();
        return new FunDef.Parameter(
                new Position(
                        name.position.start,
                        type.position.end
                ),
                name.lexeme,
                type
        );
    }

    private Expr parseExpression() {
        dump("expression -> logical_ior_expression expression2");
        Expr expression = parseLogicalIORExpression();
        Expr where = parseExpression2(expression);
        return where;
    }

    private Expr parseExpression2(Expr expression) {
        if (getSymbol().tokenType == OP_LBRACE) {
            dump("expression2 -> { WHERE definitions }");
            skipSymbol();
            if (getSymbol().tokenType != KW_WHERE)
                Report.error("Expression2\n" + "Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: WHERE");
            skipSymbol();
            Defs defs = parseDefinitions();
            if (getSymbol().tokenType != OP_RBRACE)
                Report.error("Expression2\n" + "Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: '}'");
            Position end = getSymbol().position;
            skipSymbol();
            return new Where(
                    new Position(
                            expression.position.start,
                            end.end
                    ),
                    expression,
                    defs
            );
        } else
            dump("expression2 -> e");
        return expression;
    }

    private Expr parseLogicalIORExpression() {
        dump("logical_ior_expression -> logical_and_expression logical_ior_expression2");
        Expr left = parseLogicalANDExpression();
        Expr right = parseLogicalIORExpression2();
        if (right == null)
            return left;
        return new Binary(
                new Position(
                        left.position.start,
                        right.position.end
                ),
                left,
                Binary.Operator.OR,
                right
        );
    }

    private Expr parseLogicalIORExpression2() {
        if (getSymbol().tokenType == OP_OR) {
            dump("logical_ior_expression2 -> | logical_and_expression logical_ior_expression2");
            skipSymbol();
            Expr left = parseLogicalANDExpression();
            Expr right = parseLogicalIORExpression2();
            if (right == null)
                return left;
            return new Binary(
                    new Position(
                            left.position.start,
                            right.position.end
                    ),
                    left,
                    Binary.Operator.OR,
                    right
            );
        } else
            dump("logical_ior_expression2 -> e");
        return null;
    }

    private Expr parseLogicalANDExpression() {
        dump("logical_and_expression -> compare_expression logical_and_expression2");
        parseCompareExpression();
        parseLogicalANDExpression2();
        return null;
    }

    private void parseLogicalANDExpression2() {
        if (getSymbol().tokenType == OP_AND) {
            dump("logical_and_expression2 -> \"&\" compare_expression logical_and_expression2");
            skipSymbol();
            parseCompareExpression();
            parseLogicalANDExpression2();
        } else
            dump("logical_and_expression2 -> e");
    }

    private void parseCompareExpression() {
        dump("compare_expression -> additive_expression compare_expression2");
        parseAdditiveExpression();
        parseCompareExpression2();
    }

    private void parseCompareExpression2() {
        switch (getSymbol().tokenType) {
            case OP_EQ -> dump("compare_expression2 -> == additive_expression");
            case OP_NEQ -> dump("compare_expression2 -> != additive_expression");
            case OP_LEQ -> dump("compare_expression2 -> <= additive_expression");
            case OP_GEQ -> dump("compare_expression2 -> >= additive_expression");
            case OP_LT -> dump("compare_expression2 -> < additive_expression");
            case OP_GT -> dump("compare_expression2 -> > additive_expression");
            default -> {
                dump("compare_expression2 -> e");
                return;
            }
        }
        skipSymbol();
        parseAdditiveExpression();
    }

    private void parseAdditiveExpression() {
        dump("additive_expression -> multiplicative_expression additive_expression2");
        parseMultiplicativeExpression();
        parseAdditiveExpression2();
    }

    private void parseAdditiveExpression2() {
        switch (getSymbol().tokenType) {
            case OP_ADD -> dump("additive_expression2 -> + multiplicative_expression additive_expression2");
            case OP_SUB -> dump("additive_expression2 -> - multiplicative_expression additive_expression2");
            default -> {
                dump("additive_expression2 -> e");
                return;
            }
        }
        skipSymbol();
        parseMultiplicativeExpression();
        parseAdditiveExpression2();
    }

    private void parseMultiplicativeExpression() {
        dump("multiplicative_expression -> prefix_expression multiplicative_expression2");
        parsePrefixExpression();
        parseMultiplicativeExpression2();
    }

    private void parseMultiplicativeExpression2() {
        switch (getSymbol().tokenType) {
            case OP_MUL -> dump("multiplicative_expression2 -> * prefix_expression multiplicative_expression2");
            case OP_DIV -> dump("multiplicative_expression2 -> / prefix_expression multiplicative_expression2");
            case OP_MOD -> dump("multiplicative_expression2 -> % prefix_expression multiplicative_expression2");
            default -> {
                dump("multiplicative_expression2 -> e");
                return;
            }
        }
        skipSymbol();
        parsePrefixExpression();
        parseMultiplicativeExpression2();
    }

    private void parsePrefixExpression() {
        switch (getSymbol().tokenType) {
            case OP_ADD -> dump("prefix_expression -> + prefix_expression");
            case OP_SUB -> dump("prefix_expression -> - prefix_expression");
            case OP_NOT -> dump("prefix_expression -> ! prefix_expression");
            default -> {
                dump("prefix_expression -> postfix_expression");
                parsePostfixExpression();
                return;
            }
        }
        skipSymbol();
        parsePrefixExpression();
    }

    private void parsePostfixExpression() {
        dump("postfix_expression -> atom_expression postfix_expression2");
        parseAtomExpression();
        parsePostfixExpression2();
    }

    private void parsePostfixExpression2() {
        if (getSymbol().tokenType == OP_LBRACKET) {
            dump("postfix_expression2 -> [ expression ] postfix_expression2");
            skipSymbol();
            parseExpression();
            if (getSymbol().tokenType != OP_RBRACKET)
                Report.error("PostfixExpression2\n" + "Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: ']'");
            skipSymbol();
            parsePostfixExpression2();
        } else
            dump("postfix_expression2 -> e");
    }

    private void parseAtomExpression() {
        switch (getSymbol().tokenType) {
            case C_LOGICAL -> dump("atom_expression -> log_constant");
            case C_INTEGER -> dump("atom_expression -> int_constant");
            case C_STRING -> dump("atom_expression -> str_constant");
            case IDENTIFIER -> {
                dump("atom_expression -> identifier atom_expression2");
                skipSymbol();
                // tle morš bit pozorn na skip znaka
                parseAtomExpression2();
            }
            case OP_LBRACE -> {
                dump("atom_expression -> { atom_expression3 }");
                skipSymbol();
                parseAtomExpression3();
                if (getSymbol().tokenType != OP_RBRACE)
                    Report.error("AtomExpression\n" + "Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: '}'");
            }
            case OP_LPARENT -> {
                dump("atom_expression -> ( expression )");
                skipSymbol();
                parseExpressions();
                if (getSymbol().tokenType != OP_RPARENT)
                    Report.error("AtomExpression\n" + "Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: ')'");
            }
            default ->
                    Report.error("AtomExpression\n" + "Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected:\nC_LOGICAL\nC_INTEGER\nC_STRING\nIDENTIFIER\nOP_LBRACE\nOP_LPARENT");
        }
        skipSymbol();
    }

    private void parseAtomExpression2() {
        if (getSymbol().tokenType == OP_LPARENT) {
            dump("atom_expression2 -> ( expressions )");
            skipSymbol();
            parseExpressions();
            if (getSymbol().tokenType != OP_RPARENT)
                Report.error("AtomExpression2\n" + "Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: ')'");
        } else {
            dump("atom_expression2 -> e");
            this.index--;
        }
    }

    private void parseAtomExpression3() {
        switch (getSymbol().tokenType) {
            case KW_IF -> {
                dump("atom_expression3 -> if expression then expression atom_expression4");
                skipSymbol();
                parseExpression();
                if (getSymbol().tokenType != KW_THEN)
                    Report.error("AtomExpression3\n" + "Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: KW_THEN");
                skipSymbol();
                parseExpression();
                parseAtomExpression4();
            }
            case KW_WHILE -> {
                dump("atom_expression3 -> while expression : expression");
                skipSymbol();
                parseExpression();
                if (getSymbol().tokenType != OP_COLON)
                    Report.error("AtomExpression3\n" + "Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: ':'");
                skipSymbol();
                parseExpression();
            }
            case KW_FOR -> {
                dump("atom_expression3 -> for identifier = expression , expression , expression : expression");
                skipSymbol();
                if (getSymbol().tokenType != IDENTIFIER)
                    Report.error("AtomExpression3\n" + "Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: IDENTIFIER");
                skipSymbol();
                if (getSymbol().tokenType != OP_ASSIGN)
                    Report.error("AtomExpression3\n" + "Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: '='");
                skipSymbol();
                parseExpression();
                if (getSymbol().tokenType != OP_COMMA)
                    Report.error("AtomExpression3\n" + "Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: ','");
                skipSymbol();
                parseExpression();
                if (getSymbol().tokenType != OP_COMMA)
                    Report.error("AtomExpression3\n" + "Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: ','");
                skipSymbol();
                parseExpression();
                if (getSymbol().tokenType != OP_COLON)
                    Report.error("AtomExpression3\n" + "Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: ':'");
                skipSymbol();
                parseExpression();
            }
            default -> {
                dump("atom_expression3 -> expression = expression");
                parseExpression();
                if (getSymbol().tokenType != OP_ASSIGN)
                    Report.error("AtomExpression3\n" + "Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: '='");
                skipSymbol();
                parseExpression();
            }
        }
    }

    private void parseAtomExpression4() {
        if (getSymbol().tokenType == KW_ELSE) {
            dump("atom_expression4 -> else expression");
            skipSymbol();
            parseExpression();
        } else
            dump("atom_expression4 -> e");
    }

    private void parseExpressions() {
        dump("expressions -> expression expressions2");
        parseExpression();
        parseExpressions2();
    }

    private void parseExpressions2() {
        if (getSymbol().tokenType == OP_COMMA) {
            dump("expressions2 -> , expression expressions2");
            skipSymbol();
            parseExpression();
            parseExpressions2();
        } else
            dump("expressions2 -> e");
    }

    private VarDef parseVariableDefinition(Position start) {
        dump("variable_definition -> var identifier : type");
        if (getSymbol().tokenType != IDENTIFIER)
            Report.error("VariableDefinition\n" + "Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: IDENTIFIER");
        String name = getSymbol().lexeme;
        skipSymbol();
        if (getSymbol().tokenType != OP_COLON)
            Report.error("VariableDefinition\n" + "Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: ':'");
        skipSymbol();
        Type type = parseType();
        return new VarDef(
                new Position(
                        start.start,
                        type.position.end
                ),
                name,
                type
        );
    }

    /**
     * Izpiše produkcijo na izhodni tok.
     */
    private void dump(String production) {
        if (productionsOutputStream.isPresent()) {
            productionsOutputStream.get().println(production);
        }
    }

    /**
     * Gets current symbol from the list of Symbols.
     *
     * @return Symbol.
     */
    private Symbol getSymbol() {
        return this.symbols.get(this.index);
    }

    /**
     * Advances to the next symbol.
     */
    private void skipSymbol() {
        this.index++;
    }

    /**
     * Gets current symbol and advances to the next symbol
     *
     * @return Current Symbol
     */
    private Symbol nextSymbol() {
        return this.symbols.get(this.index++);
    }
}
