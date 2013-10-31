package ru.georgeee.mathlogic.propositionalcalculus.expression;

import ru.georgeee.mathlogic.propositionalcalculus.Proof;
import ru.georgeee.mathlogic.propositionalcalculus.expression.operator.Not;

import java.io.PrintWriter;
import java.util.Map;
import java.util.Set;

/**
 * Created with IntelliJ IDEA.
 * User: georgeee
 * Date: 07.10.13
 * Time: 3:45
 * To change this template use File | Settings | File Templates.
 */
public class Variable extends Expression {
    protected final String varName;

    public Variable(String varName) {
        this.varName = varName;
    }

    @Override
    public void proveExpression(Proof proof, Map<String, Boolean> variableMapping) {
        proof.addCheckTautology(evaluate(variableMapping)?this:negate());
    }

    @Override
    public String toString() {
        return varName;
    }

    @Override
    public void appendToStringBuilder(StringBuilder sb) {
        sb.append(varName);
    }

    @Override
    public void printToPrintWriter(PrintWriter printWriter) {
        printWriter.print(varName);
    }

    @Override
    public boolean equals(Object o) {
        if(o==null) return false;
        if(o.hashCode()!=hashCode()) return false;
        if (this == o) return true;
        if (!(o instanceof Variable)) return false;

        Variable variable = (Variable) o;

        if (!varName.equals(variable.varName)) return false;

        return true;
    }

    @Override
    public int hashCodeImpl() {
        return varName.hashCode()^0xab4374fd;
    }

    @Override
    public boolean evaluate(Map<String, Boolean> variableMapping) {
        return variableMapping.get(varName);
    }

    @Override
    public Expression replaceVarsWithExpressions(Map<String, Expression> substitution) {
        Expression replacement = substitution.get(varName);
        if (replacement != null) return replacement;
        return this;
    }

    @Override
    public Expression negateImpl() {
        return ExpressionHolder.instance().getNotExpression(this);
    }

    @Override
    public void digVariables(Set<String> variableHolder) {
        variableHolder.add(varName);
    }
}
