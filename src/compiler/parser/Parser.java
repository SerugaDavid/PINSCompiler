/**
 * @Author: turk
 * @Description: Sintaksni analizator.
 */

package compiler.parser;

import static compiler.lexer.TokenType.*;
import static common.RequireNonNull.requireNonNull;

import java.io.PrintStream;
import java.util.List;
import java.util.Optional;

import common.Report;
import compiler.lexer.Position;
import compiler.lexer.Symbol;
import compiler.lexer.TokenType;

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
    public void parse() {
        parseSource();
    }

    private void parseSource() {
        dump("source -> definitions");
        parseDefinitions();
    }

    private void parseDefinitions() {
        dump("definitions -> definition definitions2");
        parseDefinition();
        parseDefinitions2();
    }

    private void parseDefinitions2() {
        if (getSymbol().tokenType == OP_SEMICOLON) {
            dump("definitions2 -> ; definition definitions2");
            skipSymbol();
            parseDefinition();
            parseDefinitions2();
        } else if (getSymbol().tokenType == EOF) {
            dump("definitions2 -> e");
            return;
        } else {
            Report.error("Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\n\nExprected: ';'");
        }
    }

    private void parseDefinition() {
        boolean executed;
        executed = parseTypeDefinition();
        if (executed)
            return;
        executed = parseFunctionDefinition();
        if (executed)
            return;
        parseVariableDefinition();
    }

    private boolean parseTypeDefinition() {
        if (getSymbol().tokenType == KW_TYP) {
            skipSymbol();
            dump("definition -> type typeDefinition");
            dump("type_definition -> typ identifier : type");
            if (getSymbol().tokenType != IDENTIFIER)
                Report.error("Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: IDENTIFIER");
            skipSymbol();
            if (getSymbol().tokenType != OP_COLON)
                Report.error("Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: ':'");
            skipSymbol();
            parseType();
            return true;
        }
        return false;
    }

    private void parseType() {
        switch (getSymbol().tokenType) {
            case IDENTIFIER:
                dump("type -> identifier");
                break;
            case AT_LOGICAL:
                dump("type -> logical");
                break;
            case AT_INTEGER:
                dump("type -> integer");
                break;
            case AT_STRING:
                dump("type -> string");
                break;
            case KW_ARR:
                dump("type -> arr [ int_const ] type");
                if (getSymbol().tokenType != OP_LBRACKET)
                    Report.error("Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: '['");
                skipSymbol();
                if (getSymbol().tokenType != C_INTEGER)
                    Report.error("Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: C_INTEGER");
                skipSymbol();
                if (getSymbol().tokenType != OP_RBRACKET)
                    Report.error("Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: ']'");
                skipSymbol();
                parseType();
            default:
                Report.error("Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected:\nIDENTIFIER\n¸AT_LOGICAL\nAT_INTEGER\nAT_STRING\nKW_ARR");
        }
    }

    private boolean parseFunctionDefinition() {
        if (getSymbol().tokenType == KW_FUN) {
            skipSymbol();
            dump("definition -> function_definition");
            dump("function_definition -> fun identifier ( parameters ) : type = expression");
            if (getSymbol().tokenType != IDENTIFIER)
                Report.error("Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: IDENTIFIER");
            skipSymbol();
            if (getSymbol().tokenType != OP_LPARENT)
                Report.error("Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: '('");
            skipSymbol();
            parseParameters();
            if (getSymbol().tokenType != OP_RPARENT)
                Report.error("Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: ')'");
            if (getSymbol().tokenType != OP_COLON)
                Report.error("Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: ':'");
            parseType();
            if (getSymbol().tokenType != OP_ASSIGN)
                Report.error("Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: '='");
            parseExpression();
            return true;
        }
        return false;
    }

    private void parseParameters() {
        dump("parameters -> parameter parameters2");
        parseParameter();
        parseParameters2();
    }

    private void parseParameters2() {
        if (getSymbol().tokenType == OP_COMMA) {
            skipSymbol();
            dump("parameters2 -> , parameter parameters2");
            parseParameter();
            parseParameters2();
        } else
            dump("parameters2 -> e");
    }

    private void parseParameter() {
        dump("parameter -> identifier : type");
        if (getSymbol().tokenType != IDENTIFIER)
            Report.error("Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: IDENTIFIER");
        skipSymbol();
        if (getSymbol().tokenType != OP_COLON)
            Report.error("Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: ':'");
        skipSymbol();
        parseType();
    }

    private void parseExpression() {
        dump("expression -> logical_ior_expression expression2");
        parseLogicalIORExpression();
        parseExpression2();
    }

    private void parseExpression2() {
        if (getSymbol().tokenType == OP_LBRACE) {
            dump("expression2 -> { WHERE definitions }");
            skipSymbol();
            if (getSymbol().tokenType != KW_WHERE)
                Report.error("Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: WHERE");
            skipSymbol();
            parseDefinitions();
            if (getSymbol().tokenType != OP_RBRACE)
                Report.error("Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: '}'");
            skipSymbol();
        } else
            dump("expression2 -> e");
    }

    private void parseLogicalIORExpression() {
        dump("logical_ior_expression -> logical_and_expression logical_ior_expression2");
        parseLogicalANDExpression();
        parseLogicalIORExpression2();
    }

    private void parseLogicalIORExpression2() {
        if (getSymbol().tokenType == OP_OR) {
            dump("logical_ior_expression2 -> | logical_and_expression logical_ior_expression2");
            skipSymbol();
            parseLogicalANDExpression();
            parseLogicalIORExpression2();
        } else
            dump("logical_ior_expression2 -> e");
    }

    private void parseLogicalANDExpression() {
        dump("logical_and_expression -> compare_expression logical_and_expression2");
        parseCompareExpression();
        parseLogicalANDExpression2();
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
                Report.error("Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: ']'");
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
                    Report.error("Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: '}'");
            }
            case OP_LPARENT -> {
                dump("atom_expression -> ( expression )");
                skipSymbol();
                parseExpression();
                if (getSymbol().tokenType != OP_RPARENT)
                    Report.error("Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: ')'");
            }
            default -> Report.error("Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected:\nC_LOGICAL\nC_INTEGER\nC_STRING\nIDENTIFIER\nOP_LBRACE\nOP_LPARENT");
        }
        skipSymbol();
    }

    private void parseAtomExpression2() {
        if (getSymbol().tokenType == OP_LPARENT) {
            dump("atom_expression2 -> ( expressions )");
            skipSymbol();
            parseExpressions();
            if (getSymbol().tokenType != OP_RPARENT)
                Report.error("Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: ')'");
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
                    Report.error("Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: KW_THEN");
                skipSymbol();
                parseExpression();
                parseAtomExpression4();
            }
            case KW_WHILE -> {
                dump("atom_expression3 -> while expression : expression");
                skipSymbol();
                parseExpression();
                if (getSymbol().tokenType != OP_COLON)
                    Report.error("Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: ':'");
                skipSymbol();
                parseExpression();
            }
            case KW_FOR -> {
                dump("atom_expression3 -> for identifier = expression , expression , expression : expression");
                skipSymbol();
                if (getSymbol().tokenType != IDENTIFIER)
                    Report.error("Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: IDENTIFIER");
                skipSymbol();
                if (getSymbol().tokenType != OP_ASSIGN)
                    Report.error("Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: '='");
                skipSymbol();
                parseExpression();
                if (getSymbol().tokenType != OP_COMMA)
                    Report.error("Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: ','");
                skipSymbol();
                parseExpression();
                if (getSymbol().tokenType != OP_COMMA)
                    Report.error("Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: ','");
                skipSymbol();
                parseExpression();
                if (getSymbol().tokenType != OP_COLON)
                    Report.error("Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: ':'");
                skipSymbol();
                parseExpression();
            }
            default -> {
                dump("atom_expression3 -> expression = expression");
                parseExpression();
                if (getSymbol().tokenType != OP_ASSIGN)
                    Report.error("Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: '='");
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

    private void parseVariableDefinition() {
        if (getSymbol().tokenType != KW_VAR)
            Report.error("Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected:\nTYP\nFUN\nVAR");
        skipSymbol();
        dump("variable_definition -> var identifier : type");
        if (getSymbol().tokenType != IDENTIFIER)
            Report.error("Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: IDENTIFIER");
        skipSymbol();
        if (getSymbol().tokenType != OP_COLON)
            Report.error("Unexpected token: " + getSymbol().tokenType + "\nat: " + getSymbol().position + "\nExpected: ':'");
        skipSymbol();
        parseType();
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
     * @return Current Symbol
     */
    private Symbol nextSymbol() {
        return this.symbols.get(this.index++);
    }
}
