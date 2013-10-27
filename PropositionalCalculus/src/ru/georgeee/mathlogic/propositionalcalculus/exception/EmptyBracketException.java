package ru.georgeee.mathlogic.propositionalcalculus.exception;

/**
 * Created with IntelliJ IDEA.
 * User: georgeee
 * Date: 05.10.13
 * Time: 16:53
 * To change this template use File | Settings | File Templates.
 */
public class EmptyBracketException extends CalcException {
    public EmptyBracketException() {
    }

    public EmptyBracketException(String detailMessage) {
        super(detailMessage);
    }

    public EmptyBracketException(String detailMessage, Throwable throwable) {
        super(detailMessage, throwable);
    }

    public EmptyBracketException(Throwable throwable) {
        super(throwable);
    }
}
