package ru.georgeee.mathlogic.propositionalcalculus.expression.operator;

import ru.georgeee.mathlogic.propositionalcalculus.expression.Expression;
import ru.georgeee.mathlogic.propositionalcalculus.expression.StringConstants;
import ru.georgeee.mathlogic.propositionalcalculus.expression.UnaryOperator;

import java.util.Map;

/**
 * Created with IntelliJ IDEA.
 * User: georgeee
 * Date: 07.10.13
 * Time: 3:40
 * To change this template use File | Settings | File Templates.
 */
public class Not extends UnaryOperator {
    public Not(Expression operand) {
        super(operand);
    }

    @Override
    protected String getOperationStringRepresentation() {
        return StringConstants.NOT_OPERATION;
    }

    @Override
    protected boolean evaluateImpl(boolean value) {
        return !value;
    }
}
