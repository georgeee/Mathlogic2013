package ru.georgeee.mathlogic.propositionalcalculus.expression;

import ru.georgeee.mathlogic.propositionalcalculus.expression.operator.Not;

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
    public String toStringImpl() {
        return varName;
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
        return new Not(this);
    }

    @Override
    public void digVariables(Set<String> variableHolder) {
        variableHolder.add(varName);
    }
}
