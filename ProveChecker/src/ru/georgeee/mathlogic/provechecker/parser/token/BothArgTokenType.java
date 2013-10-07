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
public abstract class BothArgTokenType implements TokenType {
    @Override
    public Expression getExpression(Expression leftOperand, Expression rightOperand) {
        if (leftOperand == null) {
            throw new WrongOperandsException("Left operand passed null for both assoc operator: " + getClass().toString());
        }
        if (rightOperand == null) {
            throw new WrongOperandsException("Right operand passed null for both assoc operator: " + getClass().toString());
        }
        return getExpressionImpl(leftOperand, rightOperand);
    }

    protected abstract Expression getExpressionImpl(Expression leftOperand, Expression rightOperand);

    @Override
    public boolean isLeftOperandUsed() {
        return true;
    }

    @Override
    public boolean isRightOperandUsed() {
        return true;
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
