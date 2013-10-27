package ru.georgeee.mathlogic.propositionalcalculus;

import ru.georgeee.mathlogic.propositionalcalculus.expression.AxiomSchemeExpression;
import ru.georgeee.mathlogic.propositionalcalculus.expression.Expression;
import ru.georgeee.mathlogic.propositionalcalculus.expression.operator.Implication;
import ru.georgeee.mathlogic.propositionalcalculus.parser.token.TokenHolder;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashMap;
import java.util.Iterator;

/**
 * Created with IntelliJ IDEA.
 * User: georgeee
 * Date: 07.10.13
 * Time: 6:23
 * To change this template use File | Settings | File Templates.
 */
public class Proof {
    HashMap<Expression, Entry> tautologies = new HashMap<Expression, Entry>();
    HashMap<Expression, Integer> assumptions = new HashMap<Expression, Integer>();
    ArrayList<Expression> assumptionList = new ArrayList<Expression>();
    TokenHolder tokenHolder = new TokenHolder();
    ArrayList<Entry> tautologiesList = new ArrayList<Entry>();
    HashMap<Expression, ArrayList<Implication>> mpCandidates = new HashMap<Expression, ArrayList<Implication>>();
    Expression targetExpression = null;
    final AxiomSchemeList axiomSchemeList;

    public Proof(AxiomSchemeList axiomSchemeList) {
        this.axiomSchemeList = axiomSchemeList;
    }

    public Expression getTargetExpression() {
        return targetExpression == null ? (tautologiesList.size()>0?tautologiesList.get(tautologiesList.size()-1).expression:null) : targetExpression;
    }

    public void setTargetExpression(Expression targetExpression) {
        this.targetExpression = targetExpression;
    }

    public boolean isTargetAchieved(){
        if(targetExpression == null) return true;
        return addCheckTautology(targetExpression) != null;
    }



    public Proof reduceAllAssumptions() {
        Proof proof = this;
        while (!proof.assumptions.isEmpty()) {
            proof = proof.reduceLastAssumption();
        }
        return proof;
    }

    public int getAssumptionsCount() {
        return assumptions.size();
    }

    public boolean isTautology(Expression expression) {
        return tautologies.containsKey(expression);
    }

    public Proof reduceLastAssumption() {
        return reduceAssumption(assumptionList.get(assumptionList.size()-1));
    }
    public Proof reduceAssumption(Expression assumption) {
        Proof proof = new Proof(axiomSchemeList);
        for (Expression _assumption : assumptionList) {
            if(!_assumption.equals(assumption))
            proof.addAssumption(_assumption);
        }
        for (Entry entry : tautologiesList) {
            if (entry instanceof AssumptionEntry) {
                if (entry.getExpression().equals(assumption)) {
                    axiomSchemeList.addSelfImplicationProof(proof, assumption);
                } else {
                    axiomSchemeList.addAssumptionImplicationProof(proof, assumption, entry.getExpression());
                }
            } else if (entry instanceof AxiomSchemeEntry) {
                axiomSchemeList.addAssumptionImplicationProof(proof, assumption, entry.getExpression());
            } else if (entry instanceof MPEntry) {
                MPEntry mpEntry = (MPEntry) entry;
                axiomSchemeList.addMPImplicationProof(proof, assumption, mpEntry.implication);
            }
        }
        if(targetExpression != null) proof.setTargetExpression(new Implication(assumption, targetExpression));
        return proof;
    }


    public String toString(boolean printComments, boolean reduceUnnecessaryLines) {
        boolean[] forPrint = null;
        if (reduceUnnecessaryLines) {
            forPrint = new boolean[tautologiesList.size()];
            int i = forPrint.length - 1;
            if (targetExpression != null && isTargetAchieved())
                while (!tautologiesList.get(i).getExpression().equals(targetExpression)) {
                    --i;
                }
            forPrint[i] = true;
            ++i;
            while (i > 0) {
                --i;
                if (!forPrint[i]) continue;
                Entry tautology = tautologiesList.get(i);
                if (tautology instanceof MPEntry) {
                    Implication implication = ((MPEntry) tautology).getImplication();
                    forPrint[tautologies.get(implication.getLeftOperand()).id] = true;
                    forPrint[tautologies.get(implication).id] = true;
                }
            }
        }
        StringBuilder sb = new StringBuilder();

        for(Iterator<Expression> assumptionIter = assumptionList.iterator(); assumptionIter.hasNext();){
            Expression assumption = assumptionIter.next();
            sb.append(assumption);
            if(assumptionIter.hasNext()){
                sb.append(", ");
            }
        }
        sb.append(" |- ").append(getTargetExpression()).append('\n');

        HashMap<Expression, Integer> expressionIds = new HashMap<Expression, Integer>();

        for (int i = 0; i < tautologiesList.size(); ++i) {
            if (!reduceUnnecessaryLines || forPrint[i]) {
                Entry entry = tautologiesList.get(i);
                sb.append(entry);
                if (printComments) {
                    if (entry instanceof MPEntry) {
                        sb.append(" //MP #")
                                .append(expressionIds.get(((MPEntry) entry).implication.getLeftOperand())+1)
                                .append(", #")
                                .append(expressionIds.get(((MPEntry) entry).implication)+1);
                    } else if (entry instanceof AssumptionEntry)
                        sb.append(" //Assumption #").append(assumptions.get(entry.getExpression())+1);
                    else if (entry instanceof AxiomSchemeEntry)
                        sb.append(" //Axiom scheme #").append(((AxiomSchemeEntry) entry).getAxiomSchemeExpression().getId()+1);
                }
                sb.append('\n');
                if(printComments) expressionIds.put(entry.getExpression(), expressionIds.size());
            }
        }
        return sb.toString();
    }

    @Override
    public String toString() {
        return toString(true, true);
    }

    protected void addCheckMP(Implication implication) {
        if (tautologies.containsKey(implication.getLeftOperand())) {
            if(!tautologies.containsKey(implication.getRightOperand())){
                MPEntry mpEntry = new MPEntry(implication.getRightOperand(), implication, tautologiesList.size());
                tautologies.put(implication.getRightOperand(), mpEntry);
                tautologiesList.add(mpEntry);
            }
        } else {
            ArrayList<Implication> implications = mpCandidates.get(implication.getLeftOperand());
            if(implications == null){
                implications = new ArrayList<Implication>();
                mpCandidates.put(implication.getLeftOperand(), implications);
            }
            implications.add(implication);
        }
    }

    protected void checkMPCandidates(Expression leftExpression){
        if(mpCandidates.containsKey(leftExpression)){
            ArrayList<Implication> implications = mpCandidates.remove(leftExpression);
            for(Implication implication: implications){
                addCheckMP(implication);
            }
        }
    }

    public Entry addCheckTautology(Expression expression) {
        Entry expressionEntry = null;
        if (tautologies.containsKey(expression)) {
            if (expression instanceof Implication)
                addCheckMP((Implication) expression);
            return tautologies.get(expression);
        }
        if (assumptions.containsKey(expression)) {
            expressionEntry = new AssumptionEntry(expression, tautologiesList.size());
        }
        if (expressionEntry == null) {
            AxiomSchemeExpression axiomScheme = axiomSchemeList.getMatchingAxiomScheme(expression);
            if(axiomScheme != null) expressionEntry = new AxiomSchemeEntry(expression, axiomScheme, tautologiesList.size());
        }
        if (expressionEntry != null) {
            tautologies.put(expression, expressionEntry);
            tautologiesList.add(expressionEntry);
            checkMPCandidates(expression);
            if (expression instanceof Implication)
                addCheckMP((Implication) expression);
        }
        return expressionEntry;
    }

    public Entry addCheckTautology(String source) {
        ExpressionCompiler compiler = new ExpressionCompiler(source, tokenHolder);
        return addCheckTautology(compiler.compile());
    }



    public void addAssumption(Expression assumption) {
        assumptions.put(assumption, assumptionList.size());
        assumptionList.add(assumption);
    }

    public void addAssumption(String source) {
        ExpressionCompiler compiler = new ExpressionCompiler(source, tokenHolder);
        addAssumption(compiler.compile());
    }

    public void clearAssumptions() {
        assumptions.clear();
    }

    public void clearTautologies() {
        tautologies.clear();
    }

    public static abstract class Entry {
        protected final int id;
        protected final Expression expression;

        protected Entry(Expression expression, int id) {
            this.expression = expression;
            this.id = id;
        }

        Expression getExpression() {
            return expression;
        }

        public String toString() {
            return expression.toString();
        }

    }

    public static class MPEntry extends Entry {
        protected Implication implication;

        public MPEntry(Expression expression, Implication implication, int id) {
            super(expression, id);
            this.implication = implication;
        }

        public Implication getImplication() {
            return implication;
        }
    }

    public static class AssumptionEntry extends Entry {
        public AssumptionEntry(Expression expression, int id) {
            super(expression, id);
        }
    }

    public static class AxiomSchemeEntry extends Entry {
        AxiomSchemeExpression axiomSchemeExpression;

        public AxiomSchemeEntry(Expression expression, AxiomSchemeExpression axiomSchemeExpression, int id) {
            super(expression, id);
            this.axiomSchemeExpression = axiomSchemeExpression;
        }

        public AxiomSchemeExpression getAxiomSchemeExpression() {
            return axiomSchemeExpression;
        }
    }
}
