package ru.georgeee.mathlogic.propositionalcalculus;

import ru.georgeee.mathlogic.propositionalcalculus.expression.AxiomSchemeExpression;
import ru.georgeee.mathlogic.propositionalcalculus.expression.Expression;
import ru.georgeee.mathlogic.propositionalcalculus.expression.operator.Implication;
import ru.georgeee.mathlogic.propositionalcalculus.parser.token.AxiomTokenHolder;

import java.util.ArrayList;

/**
 * Created with IntelliJ IDEA.
 * User: georgeee
 * Date: 27.10.13
 * Time: 15:31
 * To change this template use File | Settings | File Templates.
 */
public abstract class BaseAxiomSchemeList implements AxiomSchemeList {

    protected BaseAxiomSchemeList() {
        super();
    }

    ArrayList<AxiomSchemeExpression> axiomSchemes = new ArrayList<AxiomSchemeExpression>();
    AxiomTokenHolder axiomTokenHolder = new AxiomTokenHolder();

    protected void addAxiomScheme(AxiomSchemeExpression axiom) {
        axiom.setId(axiomSchemes.size());
        axiomSchemes.add(axiom);
    }

    protected void addAxiomScheme(String source) {
        ExpressionCompiler compiler = new ExpressionCompiler(source, axiomTokenHolder);
        addAxiomScheme((AxiomSchemeExpression) compiler.compile());
    }

    @Override
    public AxiomSchemeExpression getAxiomScheme(int i) {
        return axiomSchemes.get(i);
    }

    @Override
    public int size() {
        return axiomSchemes.size();
    }

    @Override
    public boolean checkAxiomSchemeMatch(int i, Expression expression) {
        axiomTokenHolder.varMapping.clear();
        return axiomSchemes.get(i).checkStructureEquals(expression);
    }

    @Override
    public AxiomSchemeExpression getMatchingAxiomScheme(Expression expression) {
        for (AxiomSchemeExpression axiomScheme : axiomSchemes) {
            axiomTokenHolder.varMapping.clear();
            if (axiomScheme.checkStructureEquals(expression)) {
                return axiomScheme;
            }
        }
        return null;
    }

}
