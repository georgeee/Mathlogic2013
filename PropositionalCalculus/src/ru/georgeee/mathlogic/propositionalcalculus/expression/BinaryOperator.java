package ru.georgeee.mathlogic.propositionalcalculus.expression;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArraySet;

/**
 * Created with IntelliJ IDEA.
 * User: georgeee
 * Date: 05.10.13
 * Time: 0:07
 * To change this template use File | Settings | File Templates.
 */
public abstract class BinaryOperator extends Expression {
    protected Expression leftOperand, rightOperand;

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
    public String toString() {
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
}
