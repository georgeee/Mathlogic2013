package ru.georgeee.mathlogic.propositionalcalculus.expression;

import ru.georgeee.mathlogic.propositionalcalculus.Proof;

import java.io.PrintWriter;
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

    @Override
    public void proveExpression(Proof proof, Map<String, Boolean> variableMapping) {
        throw new UnsupportedOperationException("Axiom scheme expressions isn't meant to be used with proveExpression facility");
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
    public String toString() {
        return expression.toString();
    }

    @Override
    public void appendToStringBuilder(StringBuilder sb) {
        expression.appendToStringBuilder(sb);
    }

    @Override
    public void printToPrintWriter(PrintWriter printWriter) {
        expression.printToPrintWriter(printWriter);
    }

    @Override
    public boolean equals(Object o) {
        if(o==null) return false;
        if(o.hashCode()!=hashCode()) return false;
        if (this == o) return true;
        if (!(o instanceof AxiomSchemeExpression)) return false;
        if (!super.equals(o)) return false;

        AxiomSchemeExpression that = (AxiomSchemeExpression) o;

        if (!expression.equals(that.expression)) return false;

        return true;
    }

    @Override
    public int hashCodeImpl() {
        return expression.hashCode()^0x4732dbfc;
    }
}
