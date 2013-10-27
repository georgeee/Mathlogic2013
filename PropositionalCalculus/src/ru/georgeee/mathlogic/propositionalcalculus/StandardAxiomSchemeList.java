package ru.georgeee.mathlogic.propositionalcalculus;

import ru.georgeee.mathlogic.propositionalcalculus.expression.AxiomSchemeExpression;
import ru.georgeee.mathlogic.propositionalcalculus.expression.Expression;
import ru.georgeee.mathlogic.propositionalcalculus.expression.operator.Implication;

/**
 * Created with IntelliJ IDEA.
 * User: georgeee
 * Date: 27.10.13
 * Time: 15:43
 * To change this template use File | Settings | File Templates.
 */
public class StandardAxiomSchemeList extends BaseAxiomSchemeList {
    private StandardAxiomSchemeList() {
        super();
        addAxiomScheme("A → B -> A");
        addAxiomScheme("(A → B) → (A → B → C) → (A → C)");
        addAxiomScheme("A → (B → (A ∧ B))");
        addAxiomScheme("(A ∧ B) → A");
        addAxiomScheme("(A ∧ B) → B");
        addAxiomScheme("A → (A ∨ B)");
        addAxiomScheme("B → (A ∨ B)");
        addAxiomScheme("(A → C) → ((B → C) → (A ∨ B → C))");
        addAxiomScheme("(A → B) → ((A → ¬B) → ¬A)");
        addAxiomScheme("¬¬A → A");
    }

    private static StandardAxiomSchemeList instance;
    public static StandardAxiomSchemeList getInstance(){
        if(instance == null){
            instance = new StandardAxiomSchemeList();
        }
        return instance;
    }
    @Override
    public void addAssumptionImplicationProof(Proof proof, Expression A, Expression Ci) {
        addCheckTautology(proof, Ci);
        Implication ACi = new Implication(A, Ci);
        addCheckTautology(proof, new Implication(Ci, ACi));
        addCheckTautology(proof, ACi);
    }

    @Override
    public void addMPImplicationProof(Proof proof, Expression A, Implication mpImplication) {
        Expression Ci = mpImplication.getRightOperand();
        Expression Cj = mpImplication.getLeftOperand();
        Implication ACi = new Implication(A, Ci);
        Implication ACj = new Implication(A, Cj);
        Implication ACjCi = new Implication(A, new Implication(Cj, Ci));
        //Axiom scheme 2
        Implication part2 = new Implication(ACjCi, ACi);
        addCheckTautology(proof, new Implication(ACj, part2));
        addCheckTautology(proof, ACj);
        addCheckTautology(proof, part2);
        addCheckTautology(proof, ACjCi);
        addCheckTautology(proof, ACi);
    }
    @Override
    public void addSelfImplicationProof(Proof proof, Expression A) {
        //Axiom scheme 2
        Implication AA = new Implication(A, A);
        Implication AiAA = new Implication(A, AA);
        Implication AiAAiA = new Implication(A, new Implication(AA, A));
        Implication part2 = new Implication(AiAAiA, AA);
        //Axiom scheme 2
        addCheckTautology(proof, new Implication(AiAA, part2));
        //Axiom scheme 1
        addCheckTautology(proof, AiAA);
        //MP 1,2
        addCheckTautology(proof, part2);
        //Axiom scheme 1
        addCheckTautology(proof, AiAAiA);
        //MP 3,4
        addCheckTautology(proof, AA);
    }
    
    protected void addCheckTautology(Proof proof, Expression expression){
        if(proof.addCheckTautology(expression) == null)
        assert proof.addCheckTautology(expression) != null;
    }
}
