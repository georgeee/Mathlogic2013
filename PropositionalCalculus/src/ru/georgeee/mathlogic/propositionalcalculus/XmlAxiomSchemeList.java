package ru.georgeee.mathlogic.propositionalcalculus;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;
import ru.georgeee.mathlogic.propositionalcalculus.expression.Expression;
import ru.georgeee.mathlogic.propositionalcalculus.expression.operator.Implication;
import ru.georgeee.mathlogic.propositionalcalculus.expression.operator.Not;
import ru.georgeee.mathlogic.propositionalcalculus.parser.token.TokenHolder;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;
import java.io.File;
import java.io.IOException;
import java.util.AbstractMap;
import java.util.Arrays;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArraySet;

/**
 * Created with IntelliJ IDEA.
 * User: georgeee
 * Date: 27.10.13
 * Time: 15:43
 * To change this template use File | Settings | File Templates.
 */
public class XmlAxiomSchemeList extends BaseAxiomSchemeList {
    protected static final String DEFAULT_AXIOM_SCHEME_LIST_RESOURCE_PATH = "res/standardAxiomSchemeList.xml";
    protected static final int DT_ASSUMPTION_IMPLICATION = 0;
    protected static final int DT_SELF_IMPLICATION = 1;
    protected static final int DT_MP_IMPLICATION = 2;
    protected static final int TERTIUM_NON_DATUR = 3;
    protected static final int AND_TT = 4;
    protected static final int AND_TF = 5;
    protected static final int AND_FT = 6;
    protected static final int AND_FF = 7;
    protected static final int OR_TT = 8;
    protected static final int OR_TF = 9;
    protected static final int OR_FT = 10;
    protected static final int OR_FF = 11;
    protected static final int IMPLICATION_TT = 12;
    protected static final int IMPLICATION_TF = 13;
    protected static final int IMPLICATION_FT = 14;
    protected static final int IMPLICATION_FF = 15;
    protected static final int NOT_T = 16;
    protected static final int NOT_F = 17;
    protected static final int ASSUMPTION_REDUCE_END = 18;
    private static XmlAxiomSchemeList standardAxiomSchemeListInstance;
    protected final ProofGenerator[] proofGenerators = new ProofGenerator[19];
    protected final TokenHolder tokenHolder;


    public XmlAxiomSchemeList(TokenHolder tokenHolder) throws IOException, SAXException, ParserConfigurationException, XPathExpressionException {
        this.tokenHolder = tokenHolder;
        DocumentBuilderFactory f = DocumentBuilderFactory.newInstance();
        f.setValidating(false);
        DocumentBuilder builder = f.newDocumentBuilder();
        Document doc = builder.parse(getClass().getResourceAsStream(DEFAULT_AXIOM_SCHEME_LIST_RESOURCE_PATH));
        readConfigsFromXMLDocument(doc);
    }

    public XmlAxiomSchemeList(Document document, TokenHolder tokenHolder) throws XPathExpressionException {
        this.tokenHolder = tokenHolder;
        readConfigsFromXMLDocument(document);
    }

    public XmlAxiomSchemeList(File xmlFile, TokenHolder tokenHolder) throws IOException, SAXException, ParserConfigurationException, XPathExpressionException {
        this.tokenHolder = tokenHolder;
        DocumentBuilderFactory f = DocumentBuilderFactory.newInstance();
        f.setValidating(false);
        DocumentBuilder builder = f.newDocumentBuilder();
        Document doc = builder.parse(xmlFile);
        readConfigsFromXMLDocument(doc);
    }

    public static XmlAxiomSchemeList getStandardAxiomSchemeListInstance() {
        if (standardAxiomSchemeListInstance == null) {
            try {
                standardAxiomSchemeListInstance = new XmlAxiomSchemeList(new TokenHolder());
            } catch (IOException e) {
                throw new RuntimeException(e);
            } catch (SAXException e) {
                throw new RuntimeException(e);
            } catch (ParserConfigurationException e) {
                throw new RuntimeException(e);
            } catch (XPathExpressionException e) {
                throw new RuntimeException(e);
            }
        }
        return standardAxiomSchemeListInstance;
    }

    protected String getElementText(String xPathString, Document document) throws XPathExpressionException {
        XPath xPath = XPathFactory.newInstance().newXPath();
        NodeList nodes = (NodeList) xPath.evaluate(xPathString, document.getDocumentElement(), XPathConstants.NODESET);
        for (int i = 0; i < nodes.getLength(); ++i) {
            Element e = (Element) nodes.item(i);
        }
        return nodes.getLength() == 0 ? null : nodes.item(0).getTextContent();
    }

    @Override
    public TokenHolder getTokenHolder() {
        return tokenHolder;
    }

    protected ProofGenerator createProofGenerator(String proofXPathSubString, Document document) throws XPathExpressionException {
        return new ProofGenerator(getElementText("/axiomSchemeList/proofs/" + proofXPathSubString, document), tokenHolder);
    }

    protected void readConfigsFromXMLDocument(Document document) throws XPathExpressionException {
        initAxiomSchemes(getElementText("/axiomSchemeList/schemeList", document));
        proofGenerators[DT_ASSUMPTION_IMPLICATION] = createProofGenerator("deductionTheorem/assumptionImplication", document);
        proofGenerators[DT_MP_IMPLICATION] = createProofGenerator("deductionTheorem/mpImplication", document);
        proofGenerators[DT_SELF_IMPLICATION] = createProofGenerator("deductionTheorem/selfImplication", document);
        proofGenerators[TERTIUM_NON_DATUR] = createProofGenerator("tertiumNonDatur", document);
        proofGenerators[AND_TT] = createProofGenerator("lemma4/and/tt", document);
        proofGenerators[AND_TF] = createProofGenerator("lemma4/and/tf", document);
        proofGenerators[AND_FT] = createProofGenerator("lemma4/and/ft", document);
        proofGenerators[AND_FF] = createProofGenerator("lemma4/and/ff", document);
        proofGenerators[OR_TT] = createProofGenerator("lemma4/or/tt", document);
        proofGenerators[OR_TF] = createProofGenerator("lemma4/or/tf", document);
        proofGenerators[OR_FT] = createProofGenerator("lemma4/or/ft", document);
        proofGenerators[OR_FF] = createProofGenerator("lemma4/or/ff", document);
        proofGenerators[IMPLICATION_TT] = createProofGenerator("lemma4/implication/tt", document);
        proofGenerators[IMPLICATION_TF] = createProofGenerator("lemma4/implication/tf", document);
        proofGenerators[IMPLICATION_FT] = createProofGenerator("lemma4/implication/ft", document);
        proofGenerators[IMPLICATION_FF] = createProofGenerator("lemma4/implication/ff", document);
        proofGenerators[NOT_T] = createProofGenerator("lemma4/not/t", document);
        proofGenerators[NOT_F] = createProofGenerator("lemma4/not/f", document);
        proofGenerators[ASSUMPTION_REDUCE_END] = createProofGenerator("assumptionReduceEnd", document);
    }

    protected ProofGenerator getProofGenerator(int generatorId) {
        return proofGenerators[generatorId];
    }

    protected Map<String, Expression> getSubstitutionMap(final AbstractMap.SimpleEntry<String, Expression>... entries) {
        return new AbstractMap<String, Expression>() {
            @Override
            public Set<Entry<String, Expression>> entrySet() {
                return new CopyOnWriteArraySet<Entry<String, Expression>>(Arrays.asList(entries));
            }
        };
    }

    @Override
    public void addAssumptionImplicationProof(Proof proof, Expression A, Expression Ci) {
        boolean operationResult = getProofGenerator(DT_ASSUMPTION_IMPLICATION).addToProof(proof, getSubstitutionMap(
                new AbstractMap.SimpleEntry<String, Expression>("A", A),
                new AbstractMap.SimpleEntry<String, Expression>("Ci", Ci)
        ));
        assert operationResult;
    }

    protected void addAssumptionReduceProofEnd(Proof proof, Expression A, Expression B) {
        boolean operationResult = getProofGenerator(ASSUMPTION_REDUCE_END).addToProof(proof, getSubstitutionMap(
                new AbstractMap.SimpleEntry<String, Expression>("A", A),
                new AbstractMap.SimpleEntry<String, Expression>("B", B)
        ));
        assert operationResult;
    }

    @Override
    public void addMPImplicationProof(Proof proof, Expression A, Implication mpImplication) {
        boolean operationResult = getProofGenerator(DT_MP_IMPLICATION).addToProof(proof, getSubstitutionMap(
                new AbstractMap.SimpleEntry<String, Expression>("A", A),
                new AbstractMap.SimpleEntry<String, Expression>("Ci", mpImplication.getRightOperand()),
                new AbstractMap.SimpleEntry<String, Expression>("Cj", mpImplication.getLeftOperand())
        ));
        assert operationResult;
    }

    @Override
    public void addSelfImplicationProof(Proof proof, Expression A) {
        boolean operationResult = getProofGenerator(DT_SELF_IMPLICATION).addToProof(proof, getSubstitutionMap(
                new AbstractMap.SimpleEntry<String, Expression>("A", A)
        ));
        assert operationResult;
    }

    @Override
    public void addTertiumNonDaturProof(Proof proof, Expression A) {
        boolean operationResult = getProofGenerator(TERTIUM_NON_DATUR).addToProof(proof, getSubstitutionMap(
                new AbstractMap.SimpleEntry<String, Expression>("A", A)
        ));
        assert operationResult;
    }

    @Override
    public void addAndOperatorProof(Proof proof, boolean leftOperandEvaluation, boolean rightOperandEvaluation, Expression leftOperand, Expression rightOperand) {
        int proofGeneratorId;
        if (leftOperandEvaluation) {
            if (rightOperandEvaluation) proofGeneratorId = AND_TT;
            else proofGeneratorId = AND_TF;
        } else {
            if (rightOperandEvaluation) proofGeneratorId = AND_FT;
            else proofGeneratorId = AND_FF;
        }
        boolean operationResult = getProofGenerator(proofGeneratorId).addToProof(proof, getSubstitutionMap(
                new AbstractMap.SimpleEntry<String, Expression>("p", leftOperand),
                new AbstractMap.SimpleEntry<String, Expression>("q", rightOperand)
        ));
        assert operationResult;
    }

    @Override
    public void addOrOperatorProof(Proof proof, boolean leftOperandEvaluation, boolean rightOperandEvaluation, Expression leftOperand, Expression rightOperand) {
        int proofGeneratorId;
        if (leftOperandEvaluation) {
            if (rightOperandEvaluation) proofGeneratorId = OR_TT;
            else proofGeneratorId = OR_TF;
        } else {
            if (rightOperandEvaluation) proofGeneratorId = OR_FT;
            else proofGeneratorId = OR_FF;
        }
        boolean operationResult = getProofGenerator(proofGeneratorId).addToProof(proof, getSubstitutionMap(
                new AbstractMap.SimpleEntry<String, Expression>("p", leftOperand),
                new AbstractMap.SimpleEntry<String, Expression>("q", rightOperand)
        ));
        assert operationResult;
    }

    @Override
    public void addImplicationOperatorProof(Proof proof, boolean leftOperandEvaluation, boolean rightOperandEvaluation, Expression leftOperand, Expression rightOperand) {
        int proofGeneratorId;
        if (leftOperandEvaluation) {
            if (rightOperandEvaluation) proofGeneratorId = IMPLICATION_TT;
            else proofGeneratorId = IMPLICATION_TF;
        } else {
            if (rightOperandEvaluation) proofGeneratorId = IMPLICATION_FT;
            else proofGeneratorId = IMPLICATION_FF;
        }
        boolean operationResult = getProofGenerator(proofGeneratorId).addToProof(proof, getSubstitutionMap(
                new AbstractMap.SimpleEntry<String, Expression>("p", leftOperand),
                new AbstractMap.SimpleEntry<String, Expression>("q", rightOperand)
        ));
        assert operationResult;
    }

    @Override
    public void addNotOperatorProof(Proof proof, boolean operandEvaluation, Expression operand) {
        int proofGeneratorId;
        if (operandEvaluation)
            proofGeneratorId = NOT_T;
        else proofGeneratorId = NOT_F;
        boolean operationResult = getProofGenerator(proofGeneratorId).addToProof(proof, getSubstitutionMap(
                new AbstractMap.SimpleEntry<String, Expression>("p", operand)
        ));
        assert operationResult;
    }

    @Override
    public Proof mergeProofs(Proof A, Proof B) {
        Expression target =  A.getTargetExpression(false);
        assert target != null;
        Expression lastAssumptionInA = A.getLastAssumption();
        Expression lastAssumptionInB = B.getLastAssumption();
        assert lastAssumptionInA != null;
        assert lastAssumptionInB != null;
        Expression assumption = null;
        if(lastAssumptionInA.equals(lastAssumptionInB.negate())){
            assumption = lastAssumptionInB;
        }else if  (lastAssumptionInB.equals(lastAssumptionInA.negate())){
            assumption = lastAssumptionInA;
        }else assert false;
        B = B.reduceLastAssumption();
        A = A.reduceLastAssumption();
        A.addAllFromProof(B);
        addTertiumNonDaturProof(A, assumption);
        addAssumptionReduceProofEnd(A, assumption, target);
        target =  A.getTargetExpression(false);
        assert target instanceof Implication;
        A.setTargetExpression(((Implication) target).getRightOperand());
        return A;
    }
}
