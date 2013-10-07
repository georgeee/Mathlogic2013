package ru.georgeee.mathlogic.provechecker.expression;

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

}
