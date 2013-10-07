package ru.georgeee.mathlogic.provechecker.expression;


/**
 * Created with IntelliJ IDEA.
 * User: georgeee
 * Date: 04.10.13
 * Time: 23:59
 * To change this template use File | Settings | File Templates.
 */

public abstract class Expression {
    @Override
    public int hashCode() {
        return (getClass().getCanonicalName() + toString()).hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        if (obj.getClass() != getClass()) return false;
        return toString().equals(obj.toString());
    }
}
