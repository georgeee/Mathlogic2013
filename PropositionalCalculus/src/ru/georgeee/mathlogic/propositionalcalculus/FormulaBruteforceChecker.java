package ru.georgeee.mathlogic.propositionalcalculus;

import ru.georgeee.mathlogic.propositionalcalculus.expression.Expression;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.TreeSet;

/**
 * Created with IntelliJ IDEA.
 * User: georgeee
 * Date: 27.10.13
 * Time: 2:46
 * To change this template use File | Settings | File Templates.
 */
public class FormulaBruteforceChecker {

    /**
     * Finds variable mapping, on which expression isn't a tautology
     * Supports up to 30 variables
     *
     * @param expression
     * @return failing mapping if exists, null otherwise
     */
    public Map<String, Boolean> findFailingVarMapping(Expression expression) {
        TreeSet<String> variableSet = new TreeSet<String>();
        expression.digVariables(variableSet);
        ArrayList<String> variableList = new ArrayList<String>(variableSet.size());
        for (String var : variableSet) variableList.add(var);
        int maxMask = 1 << variableList.size();
        HashMap<String, Boolean> varMapping = new HashMap<String, Boolean>();
        for (int mask = 0; mask < maxMask; ++mask) {
            for (int i = 0; i < variableList.size(); ++i) {
                varMapping.put(variableList.get(i), ((mask >> (i - 1)) & 1) == 1);
            }
            boolean value = expression.evaluate(varMapping);
            if (!value) return varMapping;
        }
        return null;
    }

    /**
     *
     * @param expression Caution! Expression should be a tautology
     * @return
     */
    public Proof findFormulaProof(Expression expression){
        TreeSet<String> variableSet = new TreeSet<String>();
        expression.digVariables(variableSet);
        ArrayList<String> variableList = new ArrayList<String>(variableSet.size());
        for (String var : variableSet) variableList.add(var);
        int maxMask = 1 << variableList.size();
        ArrayList<Proof> proofStack = new ArrayList<Proof>();
        ArrayList<Integer> szStack = new ArrayList<Integer>();
        HashMap<String, Boolean> varMapping = new HashMap<String, Boolean>();
        for (int mask = 0; mask < maxMask; ++mask) {
            int sz = 1;
            Proof proof = new Proof();
            proof.setTargetExpression(expression);
            for (int i = 0; i < variableList.size(); ++i) {
                String varName = variableList.get(i);
                boolean value = ((mask >> (variableList.size()-i-1)) & 1) == 1;
                varMapping.put(varName, value);
                proof.addAssumption(value ? varName : "!("+varName+")");
            }
            expression.proveExpression(proof, varMapping);
            while(!szStack.isEmpty() && szStack.get(szStack.size()-1) == sz){
                szStack.remove(szStack.size()-1);
                Proof p2 = proofStack.remove(proofStack.size() - 1);
                proof = proof.getAxiomSchemeList().mergeProofs(proof, p2);
                sz <<= 1;
            }
            szStack.add(sz);
            proofStack.add(proof);
        }
        assert proofStack.size() == 1;
        return proofStack.iterator().next();
    }
}
