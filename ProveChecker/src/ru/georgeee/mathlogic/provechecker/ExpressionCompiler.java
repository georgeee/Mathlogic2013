package ru.georgeee.mathlogic.provechecker;

import ru.georgeee.mathlogic.provechecker.exception.EmptyBracketException;
import ru.georgeee.mathlogic.provechecker.exception.TokenCompileException;
import ru.georgeee.mathlogic.provechecker.exception.UnlinkedTokensException;
import ru.georgeee.mathlogic.provechecker.exception.WrongBracketBalanceException;
import ru.georgeee.mathlogic.provechecker.expression.Expression;
import ru.georgeee.mathlogic.provechecker.parser.TokenFinder;
import ru.georgeee.mathlogic.provechecker.parser.token.Token;
import ru.georgeee.mathlogic.provechecker.parser.token.TokenHolder;

import java.util.LinkedList;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: georgeee
 * Date: 05.10.13
 * Time: 16:02
 * To change this template use File | Settings | File Templates.
 */
public class ExpressionCompiler {
    final String source;
    List<Token> tokens = null;
    Expression result = null;
    int caret;
    int maxPriority = -1;
    private final TokenHolder tokenHolder;

    public ExpressionCompiler(String source) {
        this(source, new TokenHolder());
    }

    public ExpressionCompiler(String source, TokenHolder tokenHolder) {
        this.source = source;
        this.tokenHolder = tokenHolder;

    }

    public Expression compile() {
        if (result == null) {
            TokenFinder tokenFinder = new TokenFinder(source, tokenHolder);
            tokens = tokenFinder.getTokens();
            for (Token token : tokens) {
                int priority = token.type.getPriority();
                if (priority > maxPriority) maxPriority = priority;
            }
            caret = 0;
            tokens.add(new Token(new TokenHolder.ClosingBracketTokenType()));
            result = recursiveDescentParseBrackets().expression;
        }
        return result;
    }

    protected Token recursiveDescentParseBrackets() {
        LinkedList<Token> tokens = new LinkedList<Token>();
        //Adding tokens to list
        while (true) {
            if (caret >= this.tokens.size()) throw new WrongBracketBalanceException();
            Token token = this.tokens.get(caret++);
            if (token.type.isClosingBracket()) {
                break;
            } else {
                if (token.type.isOpenningBracket()) {
                    tokens.add(recursiveDescentParseBrackets());
                } else {
                    tokens.add(token);
                }
            }
        }
        for (int priority = 0; priority <= maxPriority; ++priority) {
            for (int _i = 0; _i < tokens.size(); ++_i) {
                int i;
                if (TokenHolder.IS_RIGHT_ASSOC_LEVEL[priority]) {
                    i = tokens.size() - _i - 1;
                } else {
                    i = _i;
                }
                Token token = tokens.get(i);
                if (token.type.getPriority() == priority) {
                    Expression leftOperand = i == 0 ? null : tokens.get(i - 1).expression;
                    Expression rightOperand = i == tokens.size() - 1 ? null : tokens.get(i + 1).expression;
                    token.computeExpression(leftOperand, rightOperand);
                    if (rightOperand != null && token.type.isRightOperandUsed()) tokens.remove(i + 1);
                    if (leftOperand != null && token.type.isLeftOperandUsed()) tokens.remove(--i);
                }
                if (TokenHolder.IS_RIGHT_ASSOC_LEVEL[priority]) {
                    _i = tokens.size() - i - 1;
                } else {
                    _i = i;
                }
            }
        }
        if (tokens.isEmpty()) throw new EmptyBracketException();
        if (tokens.size() > 1) throw new UnlinkedTokensException();
        if (tokens.getFirst().expression == null) throw new TokenCompileException("Null expression resulted");
        return tokens.getFirst();
    }

}
