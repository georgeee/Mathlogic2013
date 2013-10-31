package ru.georgeee.mathlogic.propositionalcalculus.expression;

import ru.georgeee.mathlogic.propositionalcalculus.Proof;
import ru.georgeee.mathlogic.propositionalcalculus.expression.operator.Implication;
import ru.georgeee.mathlogic.propositionalcalculus.expression.operator.Not;

import java.io.PrintWriter;
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
        return ExpressionHolder.instance().getNotExpression(this);
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
    public String toString() {
        return "(" + leftOperand + getOperationStringRepresentation() + rightOperand + ")";
    }

    @Override
    public void appendToStringBuilder(StringBuilder sb) {
        sb.append('(');
        leftOperand.appendToStringBuilder(sb);
        sb.append(getOperationStringRepresentation());
        rightOperand.appendToStringBuilder(sb);
        sb.append(')');
    }

    @Override
    public void printToPrintWriter(PrintWriter printWriter) {
        printWriter.print('(');
        leftOperand.printToPrintWriter(printWriter);
        printWriter.print(getOperationStringRepresentation());
        rightOperand.printToPrintWriter(printWriter);
        printWriter.print(')');
    }

    @Override
    public boolean equals(Object o) {
        if(o==null) return false;
        if(o.hashCode()!=hashCode()) return false;
        if (this == o) return true;
        if (!(o instanceof BinaryOperator)) return false;

        BinaryOperator that = (BinaryOperator) o;

        if (!leftOperand.equals(that.leftOperand)) return false;
        if (!rightOperand.equals(that.rightOperand)) return false;

        return true;
    }

    @Override
    public int hashCodeImpl() {
        int result = leftOperand.hashCode();
        result = 2777 * result + rightOperand.hashCode();
        return result^getClassUniqueId();
    }

    protected abstract int getClassUniqueId();
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

    @Override
    public void proveExpression(Proof proof, Map<String, Boolean> variableMapping) {

        leftOperand.proveExpression(proof, variableMapping);
        rightOperand.proveExpression(proof, variableMapping);
        boolean leftOperandEvaluation = leftOperand.evaluate(variableMapping);
        boolean rightOperandEvaluation = rightOperand.evaluate(variableMapping);
        Expression needToBeProved = evaluateImpl(leftOperandEvaluation, rightOperandEvaluation)?this:negate();
        proveExpressionImpl(proof, leftOperandEvaluation, rightOperandEvaluation, leftOperand, rightOperand);
        Proof.Entry entry = proof.addCheckTautology(ExpressionHolder.instance().getImplicationExpression(rightOperandEvaluation ? rightOperand : rightOperand.negate(), needToBeProved));
        assert entry != null;
        entry = proof.addCheckTautology(needToBeProved);
        assert entry != null;
    }

    protected abstract void proveExpressionImpl(Proof proof, boolean leftOperandEvaluation,
                                                boolean rightOperandEvaluation,
                                                Expression leftOperand,
                                                Expression rightOperand);


}
