package ru.georgeee.mathlogic.propositionalcalculus;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;
import ru.georgeee.mathlogic.propositionalcalculus.expression.Expression;
import ru.georgeee.mathlogic.propositionalcalculus.expression.operator.Implication;
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
    protected final ProofGenerator[] proofGenerators = new ProofGenerator[3];
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

    protected void readConfigsFromXMLDocument(Document document) throws XPathExpressionException {
        initAxiomSchemes(getElementText("/axiomSchemeList/schemeList", document));
        String proofsBase = "/axiomSchemeList/proofs/";
        String dtBase = proofsBase + "deductionTheorem/";
        proofGenerators[DT_ASSUMPTION_IMPLICATION] = new ProofGenerator(getElementText(dtBase + "assumptionImplication", document), tokenHolder);
        proofGenerators[DT_MP_IMPLICATION] = new ProofGenerator(getElementText(dtBase + "mpImplication", document), tokenHolder);
        proofGenerators[DT_SELF_IMPLICATION] = new ProofGenerator(getElementText(dtBase + "selfImplication", document), tokenHolder);
    }

    protected ProofGenerator getProofGenerator(int generatorId) {
        return proofGenerators[generatorId];
    }

    private static XmlAxiomSchemeList standardAxiomSchemeListInstance;

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

}
