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
}
