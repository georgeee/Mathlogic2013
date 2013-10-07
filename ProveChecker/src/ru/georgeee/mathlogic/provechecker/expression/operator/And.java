package ru.georgeee.mathlogic.provechecker.expression.operator;

import ru.georgeee.mathlogic.provechecker.expression.BinaryOperator;
import ru.georgeee.mathlogic.provechecker.expression.Expression;
import ru.georgeee.mathlogic.provechecker.expression.StringConstants;

/**
 * Created with IntelliJ IDEA.
 * User: georgeee
 * Date: 07.10.13
 * Time: 3:41
 * To change this template use File | Settings | File Templates.
 */
public class And extends BinaryOperator {
    public And(Expression leftOperand, Expression rightOperand) {
        super(leftOperand, rightOperand);
    }

    @Override
    protected String getOperationStringRepresentation() {
        return StringConstants.AND_OPERATION;
    }
}
