package ru.georgeee.mathlogic.propositionalcalculus;

import junit.framework.TestCase;
import ru.georgeee.mathlogic.propositionalcalculus.expression.Expression;
import ru.georgeee.mathlogic.propositionalcalculus.expression.operator.Implication;

/**
 * Created with IntelliJ IDEA.
 * User: georgeee
 * Date: 07.10.13
 * Time: 6:43
 * To change this template use File | Settings | File Templates.
 */
public class ProofTest extends TestCase {

    private static class SingleSchemeList extends BaseAxiomSchemeList{
        public SingleSchemeList() {
            addAxiomScheme("a->(v->a)");
        }

        @Override
        public void addAssumptionImplicationProof(Proof proof, Expression A, Expression Ci) {}

        @Override
        public void addMPImplicationProof(Proof proof, Expression A, Implication mpImplication) {}

        @Override
        public void addSelfImplicationProof(Proof proof, Expression A) {}
    }

    public void testOneAxiom() throws Exception {
        Proof proof = new Proof(new SingleSchemeList());
        assertNotNull(proof.addCheckTautology("A->A->A"));
        assertNotNull(proof.addCheckTautology("A->(A->A)->A"));
        assertNull(proof.addCheckTautology("(A->(A->A))->((A->((A->A)->A))->(A->A))"));
    }

    public void testStandartAxioms() throws Exception {
        Proof proof = new Proof(StandardAxiomSchemeList.getInstance());
        assertNotNull(proof.addCheckTautology("A->A->A"));
        assertNotNull(proof.addCheckTautology("A->(A->A)->A"));
        assertNotNull(proof.addCheckTautology("(A->A->A)->(A->(A->A)->A)->(A->A)"));
        assertNotNull(proof.addCheckTautology("((A->((A->A)->A))->(A->A))"));
        assertNotNull(proof.addCheckTautology("A->A"));

        proof.clearTautologies();
        assertNotNull(proof.addCheckTautology("A->(B->(A&B))"));
        assertNotNull(proof.addCheckTautology("(A->(B->(A&B)))->(C->(A->(B->(A&B))))"));
        assertNotNull(proof.addCheckTautology("(C)->(A->(B->(A&B)))"));
        assertNull(proof.addCheckTautology("C"));
        assertNotNull(proof.addCheckTautology("A->(B->A&B)"));
    }

}
