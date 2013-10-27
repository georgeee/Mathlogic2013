package ru.georgeee.mathlogic.propositionalcalculus.parser.token;

/**
 * Created with IntelliJ IDEA.
 * User: georgeee
 * Date: 06.10.13
 * Time: 1:26
 * To change this template use File | Settings | File Templates.
 */
public interface ManualTokenTypeFactory {
    public TokenType getTokenType(String part);
}
