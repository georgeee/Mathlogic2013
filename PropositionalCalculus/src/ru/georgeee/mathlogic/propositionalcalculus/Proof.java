package ru.georgeee.mathlogic.propositionalcalculus;

import ru.georgeee.mathlogic.propositionalcalculus.expression.AxiomSchemeExpression;
import ru.georgeee.mathlogic.propositionalcalculus.expression.Expression;
import ru.georgeee.mathlogic.propositionalcalculus.expression.operator.Implication;
import ru.georgeee.mathlogic.propositionalcalculus.parser.token.TokenHolder;

import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

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
    TokenHolder tokenHolder;
    ArrayList<Entry> tautologiesList = new ArrayList<Entry>();
    HashMap<Expression, ArrayList<Implication>> mpCandidates = new HashMap<Expression, ArrayList<Implication>>();
    Expression targetExpression = null;
    final AxiomSchemeList axiomSchemeList;

    public Proof(AxiomSchemeList axiomSchemeList, TokenHolder tokenHolder) {
        this.axiomSchemeList = axiomSchemeList;
        this.tokenHolder = tokenHolder;
    }

    public Proof(AxiomSchemeList axiomSchemeList) {
        this(axiomSchemeList, axiomSchemeList.getTokenHolder());
    }

    public Proof() {
        this(XmlAxiomSchemeList.getStandardAxiomSchemeListInstance());
    }

    public TokenHolder getTokenHolder() {
        return tokenHolder;
    }

    public Expression getTargetExpression() {
        return getTargetExpression(true);
    }

    public Expression getTargetExpression(boolean returnLastIfNull) {
        if(returnLastIfNull)
            return targetExpression == null ? (tautologiesList.size() > 0 ? tautologiesList.get(tautologiesList.size() - 1).expression : null) : targetExpression;
        else return targetExpression;
    }

    public void setTargetExpression(Expression targetExpression) {
        this.targetExpression = targetExpression;
    }

    public boolean isTargetAchieved() {
        if (targetExpression == null) return true;
        return addCheckTautology(targetExpression) != null;
    }

    public void addAllFromProof(Proof proof){
        if(proof.axiomSchemeList != axiomSchemeList) throw new UnsupportedOperationException("Can't merge proofs with different axiom scheme lists");
        for(Expression assumption: proof.assumptionList){
            if(!assumptions.containsKey(assumption)){
                addAssumption(assumption);
            }
        }
        for(Map.Entry<Expression, ArrayList<Implication>> entry : proof.mpCandidates.entrySet()){
            if(mpCandidates.containsKey(entry.getKey())){
                for(Implication implication: entry.getValue())
                    mpCandidates.get(entry.getKey()).add(implication);
            }       else{
                mpCandidates.put(entry.getKey(), entry.getValue());
            }
        }
        for(Entry tautology: proof.tautologiesList){
            if(!tautologies.containsKey(tautology.getExpression())){
                Entry newEntry = tautology.cloneWithNewId(tautologiesList.size());
                tautologies.put(newEntry.getExpression(), newEntry);
                tautologiesList.add(newEntry);
            }
        }
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
        return reduceAssumption(assumptionList.get(assumptionList.size() - 1));
    }

    public Proof reduceAssumption(Expression assumption) {
        Proof proof = new Proof(axiomSchemeList, tokenHolder);
        for (Expression _assumption : assumptionList) {
            if (!_assumption.equals(assumption))
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
        if (targetExpression != null) proof.setTargetExpression(new Implication(assumption, targetExpression));
        return proof;
    }

    public AxiomSchemeList getAxiomSchemeList() {
        return axiomSchemeList;
    }

    public void writeToPrintWriter(PrintWriter out, boolean printComments, boolean reduceUnnecessaryLines){
        this.out = out;
        composeString(printComments, reduceUnnecessaryLines);
        this.out = null;
    }

    private PrintWriter out;
    private StringBuilder sb;

    private void processToOut(String string){
        if(out != null) out.print(string);
        if(sb != null) sb.append(string);
    }
    private void processToOut(char ch){
        if(out != null) out.print(ch);
        if(sb != null) sb.append(ch);
    }
    private void processToOut(Expression expression){
        if(out != null) expression.printToPrintWriter(out);
        if(sb != null) expression.appendToStringBuilder(sb);
    }

    private void processToOut(int i) {
        if(out != null) out.print(i);
        if(sb != null) sb.append(i);
    }


    private void composeString(boolean printComments, boolean reduceUnnecessaryLines){
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

        for (Iterator<Expression> assumptionIter = assumptionList.iterator(); assumptionIter.hasNext(); ) {
            Expression assumption = assumptionIter.next();
            processToOut(assumption);
            if (assumptionIter.hasNext()) {
                processToOut(", ");
            }
        }
        processToOut(" |- ");
        processToOut(getTargetExpression());
        processToOut('\n');

        HashMap<Expression, Integer> expressionIds = new HashMap<Expression, Integer>();

        for (int i = 0; i < tautologiesList.size(); ++i) {
            if (!reduceUnnecessaryLines || forPrint[i]) {
                Entry entry = tautologiesList.get(i);
                processToOut(entry.getExpression());
                if (printComments) {
                    if (entry instanceof MPEntry) {
                        processToOut(" //MP #");
                        processToOut(expressionIds.get(((MPEntry) entry).implication.getLeftOperand()) + 1);
                        processToOut(", #");
                        processToOut(expressionIds.get(((MPEntry) entry).implication) + 1);
                    } else if (entry instanceof AssumptionEntry) {
                        processToOut(" //Assumption #");
                        processToOut(assumptions.get(entry.getExpression()) + 1);
                    }else if (entry instanceof AxiomSchemeEntry){
                        processToOut(" //Axiom scheme #");
                        processToOut(((AxiomSchemeEntry) entry).getAxiomSchemeExpression().getId() + 1);
                    }else assert false;
                }
                processToOut('\n');
                if (printComments) expressionIds.put(entry.getExpression(), expressionIds.size());
            }
        }
    }

    public String toString(boolean printComments, boolean reduceUnnecessaryLines) {
        sb = new StringBuilder();
        composeString(printComments, reduceUnnecessaryLines);
        String result = sb.toString();
        sb = null;
        return result;
    }

    @Override
    public String toString() {
        return toString(true, true);
    }

    public Expression getLastAssumption(){
        return assumptionList.isEmpty()?null:assumptionList.get(assumptionList.size()-1);
    }

    protected void addCheckMP(Implication implication) {
        if (tautologies.containsKey(implication.getLeftOperand())) {
            if (!tautologies.containsKey(implication.getRightOperand())) {
                MPEntry mpEntry = new MPEntry(implication.getRightOperand(), implication, tautologiesList.size());
                tautologies.put(implication.getRightOperand(), mpEntry);
                tautologiesList.add(mpEntry);
            }
        } else {
            ArrayList<Implication> implications = mpCandidates.get(implication.getLeftOperand());
            if (implications == null) {
                implications = new ArrayList<Implication>();
                mpCandidates.put(implication.getLeftOperand(), implications);
            }
            implications.add(implication);
        }
    }

    protected void checkMPCandidates(Expression leftExpression) {
        if (mpCandidates.containsKey(leftExpression)) {
            ArrayList<Implication> implications = mpCandidates.remove(leftExpression);
            for (Implication implication : implications) {
                addCheckMP(implication);
            }
        }
    }

    public Entry addCheckTautology(Expression expression) {
        Entry expressionEntry = null;
        if (tautologies.containsKey(expression)) {
            checkMPCandidates(expression);
            if (expression instanceof Implication)
                addCheckMP((Implication) expression);
            return tautologies.get(expression);
        }
        if (assumptions.containsKey(expression)) {
            expressionEntry = new AssumptionEntry(expression, tautologiesList.size());
        }
        if (expressionEntry == null) {
            AxiomSchemeExpression axiomScheme = axiomSchemeList.getMatchingAxiomScheme(expression);
            if (axiomScheme != null)
                expressionEntry = new AxiomSchemeEntry(expression, axiomScheme, tautologiesList.size());
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
        return addCheckTautology(tokenHolder.getExpressionCompiler().compile(source));
    }


    public void addAssumption(Expression assumption) {
        assumptions.put(assumption, assumptionList.size());
        assumptionList.add(assumption);
    }

    public void addAssumption(String source) {
        addAssumption(tokenHolder.getExpressionCompiler().compile(source));
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

        public abstract Entry cloneWithNewId(int id);
    }

    public static class MPEntry extends Entry {
        protected Implication implication;

        @Override
        public MPEntry cloneWithNewId(int id) {
            return new MPEntry(expression, implication, id);
        }

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

        @Override
        public AssumptionEntry cloneWithNewId(int id) {
            return new AssumptionEntry(expression, id);
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

        @Override
        public AxiomSchemeEntry cloneWithNewId(int id) {
            return new AxiomSchemeEntry(expression, axiomSchemeExpression, id);
        }
    }
}
