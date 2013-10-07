package ru.georgeee.mathlogic.provechecker;

import junit.framework.TestCase;

/**
 * Created with IntelliJ IDEA.
 * User: georgeee
 * Date: 07.10.13
 * Time: 6:43
 * To change this template use File | Settings | File Templates.
 */
public class CheckerTest extends TestCase {


    public void testOneAxiom() throws Exception {
        Checker checker = new Checker();
        checker.addAxiom("a->(v->a)");
        assertTrue(checker.addCheckTautology("A->A->A"));
        assertTrue(checker.addCheckTautology("A->(A->A)->A"));
        assertFalse(checker.addCheckTautology("(A->(A->A))->((A->((A->A)->A))->(A->A))"));
    }

    public void testStandartAxioms() throws Exception {
        Checker checker = new Checker();
        Main.initStandardAxiomList(checker);
        assertTrue(checker.addCheckTautology("A->A->A"));
        assertTrue(checker.addCheckTautology("A->(A->A)->A"));
        assertTrue(checker.addCheckTautology("(A->A->A)->(A->(A->A)->A)->(A->A)"));
        assertTrue(checker.addCheckTautology("((A->((A->A)->A))->(A->A))"));
        assertTrue(checker.addCheckTautology("A->A"));

        checker.clearTautologies();
        assertTrue(checker.addCheckTautology("A->(B->(A&B))"));
        assertTrue(checker.addCheckTautology("(A->(B->(A&B)))->(C->(A->(B->(A&B))))"));
        assertTrue(checker.addCheckTautology("(C)->(A->(B->(A&B)))"));
        assertFalse(checker.addCheckTautology("C"));
        assertTrue(checker.addCheckTautology("A->(B->A&B)"));
    }

}
