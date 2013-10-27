package ru.georgeee.mathlogic.propositionalcalculus.expression.operator;

import ru.georgeee.mathlogic.propositionalcalculus.expression.BinaryOperator;
import ru.georgeee.mathlogic.propositionalcalculus.expression.Expression;
import ru.georgeee.mathlogic.propositionalcalculus.expression.StringConstants;

/**
 * Created with IntelliJ IDEA.
 * User: georgeee
 * Date: 07.10.13
 * Time: 3:41
 * To change this template use File | Settings | File Templates.
 */
public class Or extends BinaryOperator {
    public Or(Expression leftOperand, Expression rightOperand) {
        super(leftOperand, rightOperand);
    }

    @Override
    protected String getOperationStringRepresentation() {
        return StringConstants.OR_OPERATION;
    }

    @Override
    protected boolean evaluateImpl(boolean left, boolean right) {
        return left | right;
    }
}
