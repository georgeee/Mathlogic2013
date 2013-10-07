package ru.georgeee.mathlogic.provechecker.exception;

/**
 * Created with IntelliJ IDEA.
 * User: georgeee
 * Date: 05.10.13
 * Time: 15:51
 * To change this template use File | Settings | File Templates.
 */
public class UnknownTokenException extends CalcException {
    public UnknownTokenException() {
    }

    public UnknownTokenException(String detailMessage) {
        super(detailMessage);
    }

    public UnknownTokenException(String detailMessage, Throwable throwable) {
        super(detailMessage, throwable);
    }

    public UnknownTokenException(Throwable throwable) {
        super(throwable);
    }
}
