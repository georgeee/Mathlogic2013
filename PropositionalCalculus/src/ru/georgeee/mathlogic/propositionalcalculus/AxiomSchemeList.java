package ru.georgeee.mathlogic.propositionalcalculus;

import ru.georgeee.mathlogic.propositionalcalculus.expression.AxiomSchemeExpression;
import ru.georgeee.mathlogic.propositionalcalculus.expression.Expression;
import ru.georgeee.mathlogic.propositionalcalculus.expression.operator.Implication;
import ru.georgeee.mathlogic.propositionalcalculus.parser.token.TokenHolder;

/**
 * Created with IntelliJ IDEA.
 * User: georgeee
 * Date: 27.10.13
 * Time: 15:28
 * To change this template use File | Settings | File Templates.
 */
public interface AxiomSchemeList {
    public AxiomSchemeExpression getAxiomScheme(int i);

    public int size();

    public boolean checkAxiomSchemeMatch(int i, Expression expression);

    public AxiomSchemeExpression getMatchingAxiomScheme(Expression expression);

    public void addAssumptionImplicationProof(Proof proof, Expression A, Expression Ci);

    public void addMPImplicationProof(Proof proof, Expression A, Implication mpImplication);

    public void addSelfImplicationProof(Proof proof, Expression A);

    public void addTertiumNonDaturProof(Proof proof, Expression A);

    public void addAndOperatorProof(Proof proof, boolean leftOperandEvaluation,
                                    boolean rightOperandEvaluation,
                                    Expression leftOperand, Expression rightOperand);
    public void addOrOperatorProof(Proof proof, boolean leftOperandEvaluation,
                                   boolean rightOperandEvaluation,
                                   Expression leftOperand, Expression rightOperand);
    public void addImplicationOperatorProof(Proof proof, boolean leftOperandEvaluation,
                                            boolean rightOperandEvaluation,
                                            Expression leftOperand, Expression rightOperand);
    public void addNotOperatorProof(Proof proof, boolean operandEvaluation, Expression operand);

    public Proof mergeProofs(Proof A, Proof B);

    public TokenHolder getTokenHolder();
}
