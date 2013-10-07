package ru.georgeee.mathlogic.provechecker.parser.token;

import ru.georgeee.mathlogic.provechecker.exception.WrongOperandsException;
import ru.georgeee.mathlogic.provechecker.expression.Expression;

/**
 * Created with IntelliJ IDEA.
 * User: georgeee
 * Date: 05.10.13
 * Time: 6:36
 * To change this template use File | Settings | File Templates.
 */
public abstract class LeftArgTokenType implements TokenType {
    @Override
    public Expression getExpression(Expression leftOperand, Expression rightOperand) {
        if (leftOperand == null) {
            throw new WrongOperandsException("Left operand passed null for left assoc operator: " + getClass().toString());
        }
        return getExpressionImpl(leftOperand);
    }

    protected abstract Expression getExpressionImpl(Expression leftOperand);

    @Override
    public boolean isLeftOperandUsed() {
        return true;
    }

    @Override
    public boolean isRightOperandUsed() {
        return false;
    }

    @Override
    public boolean isOpenningBracket() {
        return false;
    }

    @Override
    public boolean isClosingBracket() {
        return false;
    }
}
