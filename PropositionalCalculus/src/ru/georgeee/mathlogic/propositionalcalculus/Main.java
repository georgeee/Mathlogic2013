package ru.georgeee.mathlogic.propositionalcalculus;

import ru.georgeee.mathlogic.propositionalcalculus.exception.CalcException;
import ru.georgeee.mathlogic.propositionalcalculus.expression.Expression;
import ru.georgeee.mathlogic.propositionalcalculus.expression.operator.Implication;
import ru.georgeee.mathlogic.propositionalcalculus.parser.token.Token;
import ru.georgeee.mathlogic.propositionalcalculus.parser.token.TokenHolder;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Map;

/**
 * Created with IntelliJ IDEA.
 * User: georgeee
 * Date: 07.10.13
 * Time: 8:40
 * To change this template use File | Settings | File Templates.
 */
public class Main {
    static final int MODE_PROOF_CHECKER = 0;
    static final int MODE_DEDUCTION_EXPANDER = 1;
    static final int MODE_PROOF_FINDER = 2;
    PrintWriter out;
    BufferedReader in;

    public Main(BufferedReader in, PrintWriter out) {
        this.in = in;
        this.out = out;
    }

    public static void main(String[] args) throws IOException {
        int mode = MODE_PROOF_FINDER;
        String inputFileName = "input.txt";
        String outputFileName = "output.txt";
        int cnt = 1;
        for (int i = 0; i < args.length; ++i) {
            String arg = args[i];
            if (arg.equals("-i")) {
                inputFileName = args[++i];
            } else if (arg.equals("-o")) {
                outputFileName = args[++i];
            } else if (arg.equals("-m")) {
                String _mode = args[++i].toLowerCase();
                if (_mode.equals("pc") || _mode.equals("c")) mode = MODE_PROOF_CHECKER;
                else if (_mode.equals("pf") || _mode.equals("f")) mode = MODE_PROOF_FINDER;
                else if (_mode.equals("de") || _mode.equals("e")) mode = MODE_DEDUCTION_EXPANDER;
                else {
                    System.err.println("Wrong mode: " + _mode);
                    return;
                }
            } else if (arg.equals("-n")) {
                Integer _cnt = Integer.parseInt(args[++i]);
                cnt = _cnt;
            } else if (arg.equals("-na")) cnt = -1;
        }
        PrintWriter out = new PrintWriter(outputFileName);
        BufferedReader in = new BufferedReader(new FileReader(inputFileName));
        Main mainInstance = new Main(in, out);
        switch (mode) {
            case MODE_PROOF_CHECKER:
                mainInstance.checkProof();
                break;
            case MODE_DEDUCTION_EXPANDER:
                mainInstance.expandDeduction(cnt);
                break;
            case MODE_PROOF_FINDER:
                mainInstance.findProof();
                break;
        }
        out.close();
        in.close();
    }

    public void findProof() throws IOException {
        String formulaSrc = in.readLine();
        FormulaBruteforceChecker bruteforceChecker = new FormulaBruteforceChecker();
        Expression formula;
        try {
            formula = new ExpressionCompiler(formulaSrc).compile();
            if (formula == null) throw new CalcException();
        } catch (CalcException ex) {
            out.println("Error occured while parsing formula!");
            return;
        }
        Map<String, Boolean> failingVarMapping = bruteforceChecker.findFailingVarMapping(formula);
        if (failingVarMapping != null) {
            out.println("Failed on var value set: " + failingVarMapping);
        }

    }

    public void checkProof() throws IOException {
        Proof proof = new Proof(StandardAxiomSchemeList.getInstance());
        String line;
        int lineId = 1;
        boolean hasError = false;
        while ((line = in.readLine()) != null) {
            line = removeComments(line);
            if (line.trim().isEmpty()) continue;
            try {
                if (proof.addCheckTautology(line) == null) {
                    hasError = true;
                    break;
                }
            } catch (CalcException ex) {
                hasError = true;
                break;
            }
            ++lineId;
        }
        if (hasError) {
            out.println("Доказательство некорректно начиная с " + lineId + " высказывания.");
        } else {
            out.println("Доказательство корректно.");
        }
    }

    protected String removeComments(String line){
        String [] parts = line.split("//");
        return parts[0];
    }

    public void expandDeduction(int cnt) throws IOException {
        String line = removeComments(in.readLine());
        String[] parts = line.split("\\|-");
        Proof proof = new Proof(StandardAxiomSchemeList.getInstance());
        proof.setTargetExpression(new ExpressionCompiler(parts[1]).compile());
        String firstLineLeftPart = parts[0].trim();
        if (!firstLineLeftPart.isEmpty()) {
            String[] _assumptions = parts[0].split(",");
            TokenHolder th = new TokenHolder();
            for (String _assumption : _assumptions) {
                proof.addAssumption(_assumption);
            }
        }
        int lineId = 1;
        while ((line = in.readLine()) != null) {
            line = removeComments(line);
            if (line.trim().isEmpty()) continue;
            try {
                if (proof.addCheckTautology(line) == null) {
                    out.println("Proof is incorrect, starting from line " + lineId);
                    return;
                }
            } catch (CalcException ex) {
                break;
            }
            ++lineId;
        }
        if(cnt < 0) proof = proof.reduceAllAssumptions();
        else{
            while(cnt > 0 && proof.getAssumptionsCount() > 0){
                proof = proof.reduceLastAssumption();
                --cnt;
            }
        }
        out.println(proof.toString(true, true));
    }


}
