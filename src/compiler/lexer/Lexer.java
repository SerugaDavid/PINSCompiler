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
        boolean[] type = {false, false, false, false};

        int line = 1;
        int column = 1;

        // loop through the source code
        for (int i = 0; i < this.source.length(); i++) {
            char c = this.source.charAt(i);

            // whitespace
            if (c == ' ' || c == '\t' || c == '\n' || c == '\r') {
                if (word != "") {
                    // take care of the word
                }
                if (c == '\n' || c == '\r') {
                    line++;
                    column = 1;
                }
                continue;
            }

            // illegal character
            if (!type[2] && isIllegal(c)) {
                Report.error("Illegal character '" + c + "' at line " + line + ", column " + column);
                continue;
            }

            // word building
            word += c;
            type = getType(word);


        }





        return symbols;
    }


    private boolean isName(String word) {
        return word.matches("[a-zA-Z_][a-zA-Z0-9_]*");
    }

    private boolean isNumber(String word) {
        return word.matches("[0-9]|[1-9][0-9]*");
    }

    private boolean canBeString(String word) {
        return isString(word) || (word.charAt(0) == '\'' && countChar(word, '\'')%2 == 1);
    }

    private boolean isString(String word) {
        return word.charAt(0) == '\'' && word.charAt(word.length() - 1) == '\'' && countChar(word, '\'')%2 == 0;
    }

    private int countChar(String word, char c) {
        int count = 0;
        for (int i = 0; i < word.length(); i++) {
            if (word.charAt(i) == c) {
                count++;
            }
        }
        return count;
    }

    private boolean isValidString(String word) {
        // TODO: implement
        return false;
    }

    private boolean isSymbol(String word) {
        String[] symbols = {"+", "-", "*", "/", "%", "&", "|", "!", "==", "!=", "<", ">", "<=", ">=", "(", ")", "[", "]", "{", "}", ":", ";", ".", ",", "="};
        for (String symbol : symbols) {
            if (word.equals(symbol)) {
                return true;
            }
        }
        return false;
    }

    private boolean[] getType(String word) {
        boolean[] type = {false, false, false, false};
        if (isName(word)) {
            type[0] = true;
        }
        if (isNumber(word)) {
            type[1] = true;
        }
        if (canBeString(word)) {
            type[2] = true;
        }
        if (isSymbol(word)) {
            type[3] = true;
        }
        return type;
    }

    private boolean isIllegal(char c) {
        return !String.valueOf(c).matches("[A-Za-z!#%->\\[\\]_{-}]");
    }
}
