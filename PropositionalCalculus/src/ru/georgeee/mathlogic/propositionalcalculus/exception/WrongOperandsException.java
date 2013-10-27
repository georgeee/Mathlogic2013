package ru.georgeee.mathlogic.propositionalcalculus.exception;

/**
 * Created with IntelliJ IDEA.
 * User: georgeee
 * Date: 05.10.13
 * Time: 6:38
 * To change this template use File | Settings | File Templates.
 */
public class WrongOperandsException extends CalcException {
    public WrongOperandsException() {
    }

    public WrongOperandsException(String detailMessage) {
        super(detailMessage);
    }
}
