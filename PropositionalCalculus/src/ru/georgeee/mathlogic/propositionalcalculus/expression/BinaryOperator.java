package ru.georgeee.mathlogic.propositionalcalculus.expression;

import ru.georgeee.mathlogic.propositionalcalculus.expression.operator.Not;

import java.util.Map;
import java.util.Set;

/**
 * Created with IntelliJ IDEA.
 * User: georgeee
 * Date: 05.10.13
 * Time: 0:07
 * To change this template use File | Settings | File Templates.
 */
public abstract class BinaryOperator extends Expression implements Cloneable {
    protected final Expression leftOperand, rightOperand;

    @Override
    protected Expression negateImpl() {
        return new Not(this);
    }

    public Expression getLeftOperand() {
        return leftOperand;
    }

    public Expression getRightOperand() {
        return rightOperand;
    }

    public BinaryOperator(Expression leftOperand, Expression rightOperand) {
        this.leftOperand = leftOperand;
        this.rightOperand = rightOperand;
    }

    protected abstract String getOperationStringRepresentation();

    @Override
    public String toStringImpl() {
        return "(" + leftOperand + getOperationStringRepresentation() + rightOperand + ")";
    }

    @Override
    public void digVariables(Set<String> variableHolder) {
        leftOperand.digVariables(variableHolder);
        rightOperand.digVariables(variableHolder);
    }


    @Override
    public boolean evaluate(Map<String, Boolean> variableMapping) {
        return evaluateImpl(leftOperand.evaluate(variableMapping), rightOperand.evaluate(variableMapping));
    }

    protected abstract boolean evaluateImpl(boolean left, boolean right);

    @Override
    public Expression replaceVarsWithExpressions(Map<String, Expression> substitution) {
        Expression newLeftOperand = leftOperand.replaceVarsWithExpressions(substitution);
        Expression newRightOperand = rightOperand.replaceVarsWithExpressions(substitution);
        if (leftOperand != newLeftOperand || rightOperand != newRightOperand)
            return createNewInstance(newLeftOperand, newRightOperand);
        return this;
    }

    protected abstract Expression createNewInstance(Expression leftOperand, Expression rightOperand);

}
