package ru.georgeee.mathlogic.propositionalcalculus.expression;

import java.util.Map;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArraySet;

/**
 * Created with IntelliJ IDEA.
 * User: georgeee
 * Date: 07.10.13
 * Time: 3:45
 * To change this template use File | Settings | File Templates.
 */
public class Variable extends Expression {
    protected String varName;

    public Variable(String varName) {
        this.varName = varName;
    }

    @Override
    public String toString() {
        return varName;
    }

    @Override
    public boolean evaluate(Map<String, Boolean> variableMapping) {
        return variableMapping.get(varName);
    }

    @Override
    public void digVariables(Set<String> variableHolder) {
        variableHolder.add(varName);
    }
}
