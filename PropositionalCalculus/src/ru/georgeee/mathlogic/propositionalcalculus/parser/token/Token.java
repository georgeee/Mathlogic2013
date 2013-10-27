package ru.georgeee.mathlogic.propositionalcalculus.parser.token;

import ru.georgeee.mathlogic.propositionalcalculus.expression.Expression;

/**
 * Created with IntelliJ IDEA.
 * User: georgeee
 * Date: 05.10.13
 * Time: 8:51
 * To change this template use File | Settings | File Templates.
 */
public class Token {
    public TokenType type = null;
    public Expression expression = null;

    public Token(TokenType type) {
        this.type = type;
    }

    public Expression computeExpression(Expression leftOperand, Expression rightOperand) {
        if (expression == null) {
            expression = type.getExpression(leftOperand, rightOperand);
        }
        return expression;
    }

}
