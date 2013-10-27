package ru.georgeee.mathlogic.propositionalcalculus.exception;

/**
 * Created with IntelliJ IDEA.
 * User: georgeee
 * Date: 05.10.13
 * Time: 17:01
 * To change this template use File | Settings | File Templates.
 */
public class TokenCompileException extends CalcException {
    public TokenCompileException() {
    }

    public TokenCompileException(String detailMessage) {
        super(detailMessage);
    }

    public TokenCompileException(String detailMessage, Throwable throwable) {
        super(detailMessage, throwable);
    }

    public TokenCompileException(Throwable throwable) {
        super(throwable);
    }
}
