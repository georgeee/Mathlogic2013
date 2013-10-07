package ru.georgeee.mathlogic.provechecker.parser.token;

import ru.georgeee.mathlogic.provechecker.expression.Expression;

/**
 * Created with IntelliJ IDEA.
 * User: georgeee
 * Date: 05.10.13
 * Time: 5:56
 * To change this template use File | Settings | File Templates.
 */
public interface TokenType {
    public Expression getExpression(Expression leftOperand, Expression rightOperand);

    public boolean isLeftOperandUsed();

    public boolean isRightOperandUsed();

    public int getPriority();

    public boolean isOpenningBracket();

    public boolean isClosingBracket();
}
