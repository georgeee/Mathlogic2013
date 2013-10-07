package ru.georgeee.mathlogic.provechecker;

import ru.georgeee.mathlogic.provechecker.expression.AxiomExpression;
import ru.georgeee.mathlogic.provechecker.expression.Expression;
import ru.georgeee.mathlogic.provechecker.expression.operator.Implication;
import ru.georgeee.mathlogic.provechecker.parser.token.AxiomTokenHolder;
import ru.georgeee.mathlogic.provechecker.parser.token.TokenHolder;

import java.util.ArrayList;
import java.util.HashSet;

/**
 * Created with IntelliJ IDEA.
 * User: georgeee
 * Date: 07.10.13
 * Time: 6:23
 * To change this template use File | Settings | File Templates.
 */
public class Checker {
    ArrayList<AxiomExpression> axioms = new ArrayList<AxiomExpression>();
    HashSet<Expression> tautologies = new HashSet<Expression>();
    AxiomTokenHolder axiomTokenHolder = new AxiomTokenHolder();
    TokenHolder tokenHolder = new TokenHolder();


    public void addAxiom(AxiomExpression axiom) {
        axioms.add(axiom);
    }

    public void checkAddMP(Expression expression) {
        if (expression instanceof Implication) {
            Implication implication = (Implication) expression;
            if (tautologies.contains(implication.getLeftOperand())) {
                tautologies.add(implication.getRightOperand());
            }
        }
    }

    public boolean addCheckTautology(Expression expression) {
        boolean isTautology = tautologies.contains(expression);
        if (!isTautology)
            for (AxiomExpression axiom : axioms) {
                axiomTokenHolder.varMapping.clear();
                if (axiom.checkStructureEquals(expression)) {
                    isTautology = true;
                    break;
                }
            }
        if (isTautology) {
            tautologies.add(expression);
            checkAddMP(expression);
        }
        return isTautology;
    }

    public boolean addCheckTautology(String source) {
        ExpressionCompiler compiler = new ExpressionCompiler(source, tokenHolder);
        return addCheckTautology(compiler.compile());
    }

    public void addAxiom(String source) {
        ExpressionCompiler compiler = new ExpressionCompiler(source, axiomTokenHolder);
        addAxiom((AxiomExpression) compiler.compile());
    }

    public void clearTautologies() {
        tautologies.clear();
    }

    public void clearAxioms() {
        tautologies.clear();
    }
}
