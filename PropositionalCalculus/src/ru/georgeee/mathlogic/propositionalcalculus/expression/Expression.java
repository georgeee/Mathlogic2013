package ru.georgeee.mathlogic.propositionalcalculus.expression;


import ru.georgeee.mathlogic.propositionalcalculus.Proof;

import java.io.PrintWriter;
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
    private int _hashCode;

    @Override
    public int hashCode() {
        if(_hashCode!=0) return _hashCode;
        _hashCode = hashCodeImpl();
        if(_hashCode==0) _hashCode = 0x9df323ee;
        return _hashCode;
    }

    public void appendToStringBuilder(StringBuilder sb){
        sb.append(toString());
    }

    public void printToPrintWriter(PrintWriter printWriter){
        printWriter.print(toString());
    }

    protected abstract int hashCodeImpl();

    public abstract void digVariables(Set<String> variableHolder);

    public abstract boolean evaluate(Map<String, Boolean> variableMapping);

    public abstract Expression replaceVarsWithExpressions(Map<String, Expression> substitution);

    public abstract void proveExpression(Proof proof, Map<String, Boolean> variableMapping);

    protected abstract Expression negateImpl();

    private Expression _negExpression;

    public Expression negate() {
        if (_negExpression == null) _negExpression = negateImpl();
        return _negExpression;
    }
}
