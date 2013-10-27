package ru.georgeee.mathlogic.propositionalcalculus.parser;

import ru.georgeee.mathlogic.propositionalcalculus.exception.UnknownTokenException;
import ru.georgeee.mathlogic.propositionalcalculus.parser.token.*;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: georgeee
 * Date: 05.10.13
 * Time: 4:16
 * To change this template use File | Settings | File Templates.
 */
public class TokenFinder {

    public final TokenType[] ahoTokenTypes;
    final protected char[] expressionChars;
    final protected String expression;
    protected LinkedList<Token> tokens = new LinkedList<Token>();
    protected AhoCorasicTrie<TokenType>.Pair[] pos;
    protected ManualTokenTypeFactory[] manualTokentTypesFabrics;
    protected AhoCorasicTrie<TokenType> trie = new AhoCorasicTrie();

    public TokenFinder(String expression, TokenHolder tokenHolder) {
        this.expression = expression;
        expressionChars = expression.toCharArray();
        pos = new AhoCorasicTrie.Pair[expressionChars.length];
        ahoTokenTypes = tokenHolder.getAhoTokenTypes();
        manualTokentTypesFabrics = tokenHolder.getManualTokenTypeFabrics();
    }

    protected void init() {
        for (TokenType tokenType : ahoTokenTypes) {
            if (tokenType instanceof AhoTokenType) {
                String word = ((AhoTokenType) tokenType).getMatchString();
                trie.addWord(word, tokenType);
            }
        }
    }

    protected void processAho() {
        AhoCorasicTrie.TrieNode node = trie.root;
        for (int i = 0; i < expressionChars.length; ++i) {
            node = node.go(expressionChars[i]);
            AhoCorasicTrie<TokenType>.Pair pair = node.getLongestMatchingPair();
            pos[i] = pair;
        }
    }

    protected TokenType getManual(String part) {
        part = part.trim();
        if (part.isEmpty()) return null;
        TokenType tokenType;
        for (ManualTokenTypeFactory factory : manualTokentTypesFabrics) {
            if ((tokenType = factory.getTokenType(part)) != null) return tokenType;
        }
        throw new UnknownTokenException("Unknown token: " + part);
    }

    protected void computeTokens() {
        for (int j = expressionChars.length - 1; j >= 0; --j) {
            int i;
            if (pos[j] != null) {
                i = j + 1 - pos[j].word.length();
                tokens.add(new Token(pos[j].data));
            } else {
                i = j;
                while (i > 0 && pos[i - 1] == null) --i;
                String manualPart = expression.substring(i, j + 1);
                TokenType manual = getManual(manualPart);
                if (manual != null) tokens.add(new Token(manual));
            }
            j = i;
        }
        Collections.reverse(tokens);
    }

    public List<Token> getTokens() {
        if (tokens != null) {
            init();
            processAho();
            computeTokens();
        }
        return tokens;
    }


}
