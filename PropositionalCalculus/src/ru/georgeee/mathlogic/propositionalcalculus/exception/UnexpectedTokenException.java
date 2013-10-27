package ru.georgeee.mathlogic.propositionalcalculus.exception;

/**
 * Created with IntelliJ IDEA.
 * User: georgeee
 * Date: 05.10.13
 * Time: 16:23
 * To change this template use File | Settings | File Templates.
 */
public class UnexpectedTokenException extends CalcException {
    public UnexpectedTokenException() {
    }

    public UnexpectedTokenException(String detailMessage) {
        super(detailMessage);
    }

    public UnexpectedTokenException(String detailMessage, Throwable throwable) {
        super(detailMessage, throwable);
    }

    public UnexpectedTokenException(Throwable throwable) {
        super(throwable);
    }
}
