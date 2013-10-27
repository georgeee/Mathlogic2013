package ru.georgeee.mathlogic.propositionalcalculus.exception;

/**
 * Created with IntelliJ IDEA.
 * User: georgeee
 * Date: 05.10.13
 * Time: 2:07
 * To change this template use File | Settings | File Templates.
 */
public class UnsupportedComputationException extends CalcException {
    public UnsupportedComputationException() {
    }

    public UnsupportedComputationException(String detailMessage) {
        super(detailMessage);
    }
}
