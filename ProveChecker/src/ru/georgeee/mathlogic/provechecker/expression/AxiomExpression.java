package ru.georgeee.mathlogic.provechecker.expression;

import java.util.Map;

/**
 * Created with IntelliJ IDEA.
 * User: georgeee
 * Date: 07.10.13
 * Time: 5:10
 * To change this template use File | Settings | File Templates.
 */
public class AxiomExpression extends Expression {
    Expression expression;
    Map<String, Expression> varMapping;

    public AxiomExpression(Expression expression, Map<String, Expression> varMapping) {
        this.varMapping = varMapping;
        this.expression = expression;
    }

    public boolean checkStructureEquals(Expression expr) {
        if (expression instanceof Variable) {
            String varName = expression.toString();
            if (varMapping.containsKey(varName)) {
                return varMapping.get(varName).equals(expr);
            } else {
                varMapping.put(varName, expr);
                return true;
            }
        } else {
            if (expr.getClass() != expression.getClass()) return false;
            if (expression instanceof UnaryOperator) {
                AxiomExpression axiomOperand = (AxiomExpression) ((UnaryOperator) expression).operand;
                Expression operand = ((UnaryOperator) expr).operand;
                return axiomOperand.checkStructureEquals(operand);
            } else if (expression instanceof BinaryOperator) {
                AxiomExpression axiomLeftOperand = (AxiomExpression) ((BinaryOperator) expression).leftOperand;
                AxiomExpression axiomRightOperand = (AxiomExpression) ((BinaryOperator) expression).rightOperand;
                Expression leftOperand = ((BinaryOperator) expr).leftOperand;
                Expression rightOperand = ((BinaryOperator) expr).rightOperand;
                return axiomLeftOperand.checkStructureEquals(leftOperand) && axiomRightOperand.checkStructureEquals(rightOperand);
            }
        }
        return false;
    }

    @Override
    public String toString() {
        return expression.toString();
    }
}
