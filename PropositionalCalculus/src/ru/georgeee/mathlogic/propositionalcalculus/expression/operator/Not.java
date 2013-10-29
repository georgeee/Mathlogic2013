package ru.georgeee.mathlogic.propositionalcalculus.expression.operator;

import ru.georgeee.mathlogic.propositionalcalculus.Main;
import ru.georgeee.mathlogic.propositionalcalculus.expression.Expression;
import ru.georgeee.mathlogic.propositionalcalculus.expression.StringConstants;
import ru.georgeee.mathlogic.propositionalcalculus.expression.UnaryOperator;

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
    public Expression negateImpl() {
        return operand;
    }

    @Override
    protected String getOperationStringRepresentation() {
        return Main.ALT_PRINT_MODE ? StringConstants.NOT_OPERATION_ALT : StringConstants.NOT_OPERATION;
    }

    @Override
    protected boolean evaluateImpl(boolean value) {
        return !value;
    }


    @Override
    protected Expression createNewInstance(Expression operand) {
        return new Not(operand);
    }
}
