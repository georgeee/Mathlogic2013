package ru.georgeee.mathlogic.propositionalcalculus.expression;

import ru.georgeee.mathlogic.propositionalcalculus.Proof;
import ru.georgeee.mathlogic.propositionalcalculus.expression.operator.Not;

import java.io.PrintWriter;
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

    @Override
    public boolean equals(Object o) {
        if(o==null) return false;
        if(o.hashCode()!=hashCode()) return false;
        if (this == o) return true;
        if (!(o instanceof UnaryOperator)) return false;

        UnaryOperator that = (UnaryOperator) o;

        if (!operand.equals(that.operand)) return false;

        return true;
    }

    @Override
    public int hashCodeImpl() {
        return operand.hashCode()^getClassUniqueId();
    }

    protected abstract int getClassUniqueId();

    public UnaryOperator(Expression operand) {
        this.operand = operand;
    }

    protected abstract String getOperationStringRepresentation();

    @Override
    public String toString() {
        return "(" + getOperationStringRepresentation() + operand + ")";
    }

    @Override
    public void appendToStringBuilder(StringBuilder sb) {
        sb.append('(');
        sb.append(getOperationStringRepresentation());
        operand.appendToStringBuilder(sb);
        sb.append(')');
    }

    @Override
    public void printToPrintWriter(PrintWriter printWriter) {
        printWriter.print('(');
        printWriter.print(getOperationStringRepresentation());
        operand.printToPrintWriter(printWriter);
        printWriter.print(')');
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

    @Override
    public Expression negateImpl() {
        return ExpressionHolder.instance().getNotExpression(this);
    }

    protected abstract boolean evaluateImpl(boolean value);

    protected abstract Expression createNewInstance(Expression operand);

    @Override
    public void proveExpression(Proof proof, Map<String, Boolean> variableMapping) {
        operand.proveExpression(proof, variableMapping);
        boolean operandEvaluation = operand.evaluate(variableMapping);
        proveExpressionImpl(proof, operandEvaluation, operand);
        proof.addCheckTautology(this);
    }

    protected abstract void proveExpressionImpl(Proof proof, boolean operandEvaluation, Expression operand);
}
