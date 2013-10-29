package ru.georgeee.mathlogic.propositionalcalculus.expression;

import java.util.Map;
import java.util.Set;

/**
 * Created with IntelliJ IDEA.
 * User: georgeee
 * Date: 05.10.13
 * Time: 0:13
 * To change this template use File | Settings | File Templates.
 */
public abstract class UnaryOperator extends Expression {
    protected Expression operand;

    public UnaryOperator(Expression operand) {
        this.operand = operand;
    }


    protected abstract String getOperationStringRepresentation();

    @Override
    public String toStringImpl() {
        return "(" + getOperationStringRepresentation() + operand + ")";
    }

    @Override
    public void digVariables(Set<String> variableHolder) {
        operand.digVariables(variableHolder);
    }

    @Override
    public boolean evaluate(Map<String, Boolean> variableMapping) {
        return evaluateImpl(operand.evaluate(variableMapping));
    }


    @Override
    public Expression replaceVarsWithExpressions(Map<String, Expression> substitution) {
        Expression newOperand = operand.replaceVarsWithExpressions(substitution);
        if (operand != newOperand)
            return createNewInstance(newOperand);
        return this;
    }

    protected abstract boolean evaluateImpl(boolean value);

    protected abstract Expression createNewInstance(Expression operand);
}
