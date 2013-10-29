package ru.georgeee.mathlogic.propositionalcalculus.expression;

import java.util.Map;
import java.util.Set;

/**
 * Created with IntelliJ IDEA.
 * User: georgeee
 * Date: 07.10.13
 * Time: 5:10
 * To change this template use File | Settings | File Templates.
 */
public class AxiomSchemeExpression extends Expression {
    int id;

    @Override
    protected Expression negateImpl() {
        throw new UnsupportedOperationException("Axiom scheme expressions isn't meant to be used with negate facility");
    }

    @Override
    public Expression replaceVarsWithExpressions(Map<String, Expression> substitution) {
        throw new UnsupportedOperationException("Axiom scheme expressions isn't meant to be used with replaceVarsWithExpressions facility");
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    Expression expression;
    Map<String, Expression> varMapping;

    public AxiomSchemeExpression(Expression expression, Map<String, Expression> varMapping) {
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
                AxiomSchemeExpression axiomOperand = (AxiomSchemeExpression) ((UnaryOperator) expression).operand;
                Expression operand = ((UnaryOperator) expr).operand;
                return axiomOperand.checkStructureEquals(operand);
            } else if (expression instanceof BinaryOperator) {
                AxiomSchemeExpression axiomLeftOperand = (AxiomSchemeExpression) ((BinaryOperator) expression).leftOperand;
                AxiomSchemeExpression axiomRightOperand = (AxiomSchemeExpression) ((BinaryOperator) expression).rightOperand;
                Expression leftOperand = ((BinaryOperator) expr).leftOperand;
                Expression rightOperand = ((BinaryOperator) expr).rightOperand;
                return axiomLeftOperand.checkStructureEquals(leftOperand) && axiomRightOperand.checkStructureEquals(rightOperand);
            }
        }
        return false;
    }

    @Override
    public void digVariables(Set<String> variableHolder) {
        expression.digVariables(variableHolder);
    }

    @Override
    public boolean evaluate(Map<String, Boolean> variableMapping) {
        return expression.evaluate(variableMapping);
    }

    @Override
    public String toStringImpl() {
        return expression.toString();
    }
}
