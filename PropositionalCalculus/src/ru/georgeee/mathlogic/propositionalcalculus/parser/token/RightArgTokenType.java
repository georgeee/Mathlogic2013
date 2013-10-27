package ru.georgeee.mathlogic.propositionalcalculus.parser.token;

import ru.georgeee.mathlogic.propositionalcalculus.exception.WrongOperandsException;
import ru.georgeee.mathlogic.propositionalcalculus.expression.Expression;

/**
 * Created with IntelliJ IDEA.
 * User: georgeee
 * Date: 05.10.13
 * Time: 6:36
 * To change this template use File | Settings | File Templates.
 */
public abstract class RightArgTokenType implements TokenType {
    @Override
    public Expression getExpression(Expression leftOperand, Expression rightOperand) {
        if (rightOperand == null) {
            throw new WrongOperandsException("Right operand passed null for right assoc operator: " + getClass().toString());
        }
        return getExpressionImpl(rightOperand);
    }

    protected abstract Expression getExpressionImpl(Expression rightOperand);

    @Override
    public boolean isLeftOperandUsed() {
        return false;
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
