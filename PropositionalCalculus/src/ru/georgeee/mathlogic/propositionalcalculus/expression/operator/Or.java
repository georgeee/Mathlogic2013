package ru.georgeee.mathlogic.propositionalcalculus.expression.operator;

import ru.georgeee.mathlogic.propositionalcalculus.Main;
import ru.georgeee.mathlogic.propositionalcalculus.Proof;
import ru.georgeee.mathlogic.propositionalcalculus.expression.BinaryOperator;
import ru.georgeee.mathlogic.propositionalcalculus.expression.Expression;
import ru.georgeee.mathlogic.propositionalcalculus.expression.ExpressionHolder;
import ru.georgeee.mathlogic.propositionalcalculus.expression.StringConstants;

/**
 * Created with IntelliJ IDEA.
 * User: georgeee
 * Date: 07.10.13
 * Time: 3:41
 * To change this template use File | Settings | File Templates.
 */
public class Or extends BinaryOperator {
    public Or(Expression leftOperand, Expression rightOperand) {
        super(leftOperand, rightOperand);
    }


    @Override
    protected int getClassUniqueId() {
        return 0x18bc4e3a;
    }
    @Override
    protected String getOperationStringRepresentation() {
        return Main.ALT_PRINT_MODE ? ' ' + StringConstants.OR_OPERATION_ALT + ' ' : StringConstants.OR_OPERATION;
    }

    @Override
    protected boolean evaluateImpl(boolean left, boolean right) {
        return left | right;
    }


    @Override
    protected Expression createNewInstance(Expression leftOperand, Expression rightOperand) {
        return ExpressionHolder.instance().getOrExpression(leftOperand, rightOperand);
    }

    @Override
    protected void proveExpressionImpl(Proof proof, boolean leftOperandEvaluation, boolean rightOperandEvaluation, Expression leftOperand, Expression rightOperand) {
        proof.getAxiomSchemeList().addOrOperatorProof(proof, leftOperandEvaluation, rightOperandEvaluation, leftOperand, rightOperand);
    }

}
