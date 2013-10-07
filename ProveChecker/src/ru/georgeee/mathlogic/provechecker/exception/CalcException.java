package ru.georgeee.mathlogic.provechecker.exception;

/**
 * Created with IntelliJ IDEA.
 * User: georgeee
 * Date: 05.10.13
 * Time: 6:39
 * To change this template use File | Settings | File Templates.
 */
public class CalcException extends RuntimeException {
    public CalcException() {
    }

    public CalcException(String detailMessage) {
        super(detailMessage);
    }

    public CalcException(String detailMessage, Throwable throwable) {
        super(detailMessage, throwable);
    }

    public CalcException(Throwable throwable) {
        super(throwable);
    }
}
