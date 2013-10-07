package ru.georgeee.mathlogic.provechecker.exception;

/**
 * Created with IntelliJ IDEA.
 * User: georgeee
 * Date: 05.10.13
 * Time: 16:47
 * To change this template use File | Settings | File Templates.
 */
public class UnlinkedTokensException extends CalcException {
    public UnlinkedTokensException() {
    }

    public UnlinkedTokensException(String detailMessage) {
        super(detailMessage);
    }

    public UnlinkedTokensException(String detailMessage, Throwable throwable) {
        super(detailMessage, throwable);
    }

    public UnlinkedTokensException(Throwable throwable) {
        super(throwable);
    }
}
