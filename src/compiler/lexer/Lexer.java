/**
 * @Author: turk
 * @Description: Leksikalni analizator.
 */

package compiler.lexer;

import static common.RequireNonNull.requireNonNull;
import static compiler.lexer.TokenType.*;
import compiler.lexer.Position.Location;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import common.Report;

public class Lexer {
    /**
     * Izvorna koda.
     */
    private final String source;

    /**
     * Preslikava iz ključnih besed v vrste simbolov.
     */
    private final static Map<String, TokenType> keywordMapping;
    private final static Map<String, TokenType> operatorMapping;

    static {
        keywordMapping = new HashMap<>();
        for (var token : TokenType.values()) {
            var str = token.toString();
            if (str.startsWith("KW_")) {
                keywordMapping.put(str.substring("KW_".length()).toLowerCase(), token);
            }
            if (str.startsWith("AT_")) {
                keywordMapping.put(str.substring("AT_".length()).toLowerCase(), token);
            }
        }

        operatorMapping = new HashMap<>();
        operatorMapping.put("+", OP_ADD);
        operatorMapping.put("-", OP_SUB);
        operatorMapping.put("*", OP_MUL);
        operatorMapping.put("/", OP_DIV);
        operatorMapping.put("%", OP_MOD);
        operatorMapping.put("&", OP_AND);
        operatorMapping.put("|", OP_OR);
        operatorMapping.put("!", OP_NOT);
        operatorMapping.put("==", OP_EQ);
        operatorMapping.put("!=", OP_NEQ);
        operatorMapping.put("=", OP_ASSIGN);
        operatorMapping.put("(", OP_LPARENT);
        operatorMapping.put(")", OP_RPARENT);
        operatorMapping.put("[", OP_LBRACKET);
        operatorMapping.put("]", OP_RBRACKET);
        operatorMapping.put("{", OP_LBRACE);
        operatorMapping.put("}", OP_RBRACE);
        operatorMapping.put(":", OP_COLON);
        operatorMapping.put(";", OP_SEMICOLON);
        operatorMapping.put(".", OP_DOT);
        operatorMapping.put(",", OP_COMMA);
        operatorMapping.put("<", OP_LT);
        operatorMapping.put(">", OP_GT);
        operatorMapping.put("<=", OP_LEQ);
        operatorMapping.put(">=", OP_GEQ);
    }

    /**
     * Ustvari nov analizator.
     * 
     * @param source Izvorna koda programa.
     */
    public Lexer(String source) {
        requireNonNull(source);
        this.source = source;
    }

    /**
     * Izvedi leksikalno analizo.
     * 
     * @return seznam leksikalnih simbolov.
     */
    public List<Symbol> scan() {
        var symbols = new ArrayList<Symbol>();

        // declare needed variables
        String word = "";
        Symbol symbol;
        // name, number, operator, canBeString, string
        boolean[] type = {false, false, false, false, false};

        int line = 1;
        int column = 1;

        // loop through the source code
        for (int i = 0; i < this.source.length(); i++) {
            char c = this.source.charAt(i);

            // comments
            if (c == '#' || (word.length() != 0 && word.charAt(0) == '#')) {
                if (word.length() != 0 && word.charAt(0) != '#') {
                    // Handle the start of a comment and render the word
                    symbol = renderWord(word, line, column);
                    symbols.add(symbol);
                    column++;
                    word = "";
                }
                if (c == '\n' || c == '\r') {
                    // end a comment at the end of the line
                    line++;
                    column = 1;
                    word = "";
                    continue;
                }
                // add the character to the comment
                word += c;
                continue;
            }

            // whitespace
            if (c == ' ' || c == '\t' || c == '\n' || c == '\r') {
                if (word != "") {
                    // render the word when whitespace is encountered
                    symbol = renderWord(word, line, column);
                    symbols.add(symbol);
                    column++;
                    word = "";
                }
                if (c == '\n' || c == '\r') {
                    // update line and column with new line
                    line++;
                    column = 1;
                }
                continue;
            }

            // illegal character
            if (!type[2] && isIllegal(c)) {
                Location start = new Location(line, column - word.length() + 1);
                Location end = new Location(line, column);
                Position pos = new Position(start, end);
                Report.error(pos, "Invalid character: '" + c + "'");
                continue;
            }

            // word building
            word += c;
            type = getType(word);
            if (!anyType(type)) {
                // render a word when a new type is encountered
                word = word.substring(0, word.length() - 1);
                symbol = renderWord(word, line, column);
                symbols.add(symbol);
                word = "";
                i--;
            } else {
                column++;
            }
        }

        // add last word
        if (word != "") {
            symbol = renderWord(word, line, column-1);
            symbols.add(symbol);
        }

        // add EOF
        symbols.add(new Symbol(new Location(line, column), new Location(line, column), EOF, "EOF"));

        return symbols;
    }

    /**
     * This method gets a word and then creates a symbol from it.
     *
     * @param word represents a word that will get rendered.
     * @param line represents the line of the word.
     * @param column represents the column on where the word ends.
     * @return Object Symbol with rendered word.
     */
    private Symbol renderWord(String word, int line, int column) {
        // initialize symbol and locations
        Symbol symbol = null;
        Location start = new Location(line, column - word.length() + 1);
        Location end = new Location(line, column);

        // check the type of the word
        boolean[] type = getType(word);
        if (type[0]) {
            // name
            TokenType token = keywordMapping.get(word);
            if (token == null) {
                if (word.equals("true") || word.equals("false"))
                    token = C_LOGICAL;
                else
                    token = IDENTIFIER;
            }
            symbol = new Symbol(start, end, token, word);
        }
        if (type[1]) {
            // number
            symbol = new Symbol(start, end, C_INTEGER, word);
        }
        if (type[2]) {
            // symbol
            symbol = new Symbol(start, end, operatorMapping.get(word), word);
        }
        if (type[4]) {
            // string
            if (isValidString(word, line, column)) {
                symbol = new Symbol(start, end, C_STRING, renderString(word));
            }
        }
        return symbol;
    }

    /**
     * This method checks if the word is a valid name or IDENTIFIER.
     *
     * @param word represents a word that will get checked.
     * @return boolean true if the word is a valid name, false otherwise.
     */
    private boolean isName(String word) {
        return word.matches("[a-zA-Z_][a-zA-Z0-9_]*");
    }

    /**
     * This method checks if the word is a valid number C_INTEGER.
     *
     * @param word represents a word that will get checked.
     * @return boolean true if the word is a valid number, false otherwise.
     */
    private boolean isNumber(String word) {
        return word.matches("[0-9]|[1-9][0-9]*");
    }

    /**
     * This method checks if the word could potentially be a string or C_STRING.
     *
     * @param word represents a word that will get checked.
     * @return boolean true if the word could be a string, false otherwise.
     */
    private boolean canBeString(String word) {
        return isString(word) || (word.charAt(0) == '\'' && countChar(word, '\'')%2 == 1);
    }

    /**
     * This method checks if the word is a valid string or C_STRING.
     *
     * @param word represents a word that will get checked.
     * @return boolean true if the word is a valid string, false otherwise.
     */
    private boolean isString(String word) {
        return word.charAt(0) == '\'' && word.charAt(word.length() - 1) == '\'' && countChar(word, '\'')%2 == 0;
    }

    /**
     * This method checks if the word is a valid string or C_STRING.
     *
     * @param word represents a word that is a string.
     * @param line represents the line of the word.
     * @param column represents the column on where the word ends.
     * @return boolean true if the word is a valid string, false otherwise.
     */
    private boolean isValidString(String word, int line, int column) {
        word = word.substring(1, word.length() - 1);
        if (!word.matches("[ -~]*")) {
            Location start = new Location(line, column - word.length() + 1);
            Location end = new Location(line, column);
            Position pos = new Position(start, end);
            Report.error(pos, "Invalid string constant");
        }
        return true;
    }

    /**
     * This method renders a string by removing the quotes and replacing the escape sequences.
     *
     * @param word that is a valid string or C_STRING.
     * @return rendered string.
     */
    private String renderString(String word) {
        // TODO: implement
        return "";
    }

    /**
     * This method counts the number of occurrences of a character in a word.
     *
     * @param word represents a string from where to count the characters.
     * @param c represents the character to count.
     * @return number of occurrences of the character in the word.
     */
    private int countChar(String word, char c) {
        int count = 0;
        for (int i = 0; i < word.length(); i++) {
            if (word.charAt(i) == c) {
                count++;
            }
        }
        return count;
    }


    /**
     * This method checks if the word is a valid operator.
     *
     * @param word represents a word that will get checked.
     * @return boolean true if the word is a valid operator, false otherwise.
     */
    private boolean isOperator(String word) {
        String[] operators = {"+", "-", "*", "/", "%", "&", "|", "!", "==", "!=", "<", ">", "<=", ">=", "(", ")", "[", "]", "{", "}", ":", ";", ".", ",", "="};
        for (String operator : operators) {
            if (word.equals(operator)) {
                return true;
            }
        }
        return false;
    }

    /**
     * This method checks the type of the word.
     *
     * @param word represents a word that will get checked.
     * @return boolean[] array of booleans that represent the type of the word.
     */
    private boolean[] getType(String word) {
        boolean[] type = {false, false, false, false, false};
        if (isName(word)) {
            type[0] = true;
        }
        if (isNumber(word)) {
            type[1] = true;
        }
        if (isOperator(word)) {
            type[2] = true;
        }
        if (canBeString(word)) {
            type[3] = true;
        }
        if (isString(word)) {
            type[4] = true;
        }
        return type;
    }

    /**
     * This method checks if the word is of any type.
     *
     * @param type boolean[] array that represent the types of the word.
     * @return true if the word is of any type, false otherwise.
     */
    private boolean anyType(boolean[] type) {
        for (boolean b : type) {
            if (b)
                return true;
        }
        return false;
    }

    /**
     * This method checks if the character is illegal in this language.
     *
     * @param c represents a character that will get checked.
     * @return true if the character is illegal, false otherwise.
     */
    private boolean isIllegal(char c) {
        return !String.valueOf(c).matches("[A-Za-z!#%->\\[\\]_{-}]");
    }
}
