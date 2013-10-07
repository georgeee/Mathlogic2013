package ru.georgeee.mathlogic.provechecker.expression;

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

}
