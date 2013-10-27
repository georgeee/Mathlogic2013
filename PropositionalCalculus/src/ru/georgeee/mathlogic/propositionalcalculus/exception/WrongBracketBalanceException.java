package ru.georgeee.mathlogic.propositionalcalculus.exception;

/**
 * Created with IntelliJ IDEA.
 * User: georgeee
 * Date: 05.10.13
 * Time: 16:51
 * To change this template use File | Settings | File Templates.
 */
public class WrongBracketBalanceException extends CalcException {
    public WrongBracketBalanceException() {
    }

    public WrongBracketBalanceException(String detailMessage) {
        super(detailMessage);
    }

    public WrongBracketBalanceException(String detailMessage, Throwable throwable) {
        super(detailMessage, throwable);
    }

    public WrongBracketBalanceException(Throwable throwable) {
        super(throwable);
    }
}
