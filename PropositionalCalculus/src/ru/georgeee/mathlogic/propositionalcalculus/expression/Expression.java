package ru.georgeee.mathlogic.propositionalcalculus.expression;


import java.util.Map;
import java.util.Set;

/**
 * Contract: all Expressions are constant.
 * All meaning properties should be final
 * <p/>
 * Created with IntelliJ IDEA.
 * User: georgeee
 * Date: 04.10.13
 * Time: 23:59
 * To change this template use File | Settings | File Templates.
 */

public abstract class Expression {
    private int _hashCode = 0;
    private String _toString = null;

    @Override
    public String toString() {
        if (_toString == null) _toString = toStringImpl();
        return _toString;
    }

    protected abstract String toStringImpl();

    @Override
    public int hashCode() {
        if (_hashCode == 0) _hashCode = (getClass().getCanonicalName() + toString()).hashCode();
        return _hashCode;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null || obj.getClass() != getClass()) return false;
        if (obj.hashCode() != hashCode()) return false;
        return toString().equals(obj.toString());
    }

    public abstract void digVariables(Set<String> variableHolder);

    public abstract boolean evaluate(Map<String, Boolean> variableMapping);

    public abstract Expression replaceVarsWithExpressions(Map<String, Expression> substitution);

    protected abstract Expression negateImpl();

    private Expression _negExpression;

    public Expression negate() {
        if (_negExpression == null) _negExpression = negateImpl();
        return _negExpression;
    }
}
