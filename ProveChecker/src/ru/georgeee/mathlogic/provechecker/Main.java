package ru.georgeee.mathlogic.provechecker;

import ru.georgeee.mathlogic.provechecker.exception.CalcException;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;

/**
 * Created with IntelliJ IDEA.
 * User: georgeee
 * Date: 07.10.13
 * Time: 8:40
 * To change this template use File | Settings | File Templates.
 */
public class Main {
    public static void main(String[] args) throws IOException {
        String inputFileName = "input.txt";
        String outputFileName = "output.txt";
        if (args.length >= 2) {
            inputFileName = args[0];
            outputFileName = args[1];
        }
        PrintWriter out = new PrintWriter(outputFileName);
        BufferedReader in = new BufferedReader(new FileReader(inputFileName));
        Checker checker = new Checker();
        initStandardAxiomList(checker);
        String line;
        int lineId = 1;
        boolean hasError = false;
        while ((line = in.readLine()) != null) {
            if(line.trim().isEmpty()) continue;
            try {
                if (!checker.addCheckTautology(line)) {
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
        out.close();
        in.close();
    }

    public static void initStandardAxiomList(Checker checker) {
        checker.addAxiom("A → B -> A");
        checker.addAxiom("(A → B) → (A → B → C) → (A → C)");
        checker.addAxiom("A → (B → (A ∧ B))");
        checker.addAxiom("(A ∧ B) → A");
        checker.addAxiom("(A ∧ B) → B");
        checker.addAxiom("A → (A ∨ B)");
        checker.addAxiom("B → (A ∨ B)");
        checker.addAxiom("(A → C) → ((B → C) → (A ∨ B → C))");
        checker.addAxiom("(A → B) → ((A → ¬B) → ¬A)");
        checker.addAxiom("¬¬A → A");
    }
}
