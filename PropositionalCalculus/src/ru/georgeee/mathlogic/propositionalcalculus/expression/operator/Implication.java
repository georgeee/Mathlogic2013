package ru.georgeee.mathlogic.propositionalcalculus.expression.operator;

import ru.georgeee.mathlogic.propositionalcalculus.Main;
import ru.georgeee.mathlogic.propositionalcalculus.Proof;
import ru.georgeee.mathlogic.propositionalcalculus.expression.BinaryOperator;
import ru.georgeee.mathlogic.propositionalcalculus.expression.Expression;
import ru.georgeee.mathlogic.propositionalcalculus.expression.StringConstants;

/**
 * Created with IntelliJ IDEA.
 * User: georgeee
 * Date: 07.10.13
 * Time: 3:41
 * To change this template use File | Settings | File Templates.
 */
public class Implication extends BinaryOperator {
    public Implication(Expression leftOperand, Expression rightOperand) {
        super(leftOperand, rightOperand);
    }

    @Override
    protected int getClassUniqueId() {
        return 0x9a5f72c5;
    }
    @Override
    protected String getOperationStringRepresentation() {
        return Main.ALT_PRINT_MODE ? ' ' + StringConstants.IMPLICATION_OPERATION_ALT + ' ' : StringConstants.IMPLICATION_OPERATION;
    }

    @Override
    protected boolean evaluateImpl(boolean left, boolean right) {
        return !left | (left & right);
    }

    @Override
    protected Expression createNewInstance(Expression leftOperand, Expression rightOperand) {
        return new Implication(leftOperand, rightOperand);
    }

    @Override
    protected void proveExpressionImpl(Proof proof, boolean leftOperandEvaluation, boolean rightOperandEvaluation, Expression leftOperand, Expression rightOperand) {
        proof.getAxiomSchemeList().addImplicationOperatorProof(proof, leftOperandEvaluation, rightOperandEvaluation, leftOperand, rightOperand);
    }
}
