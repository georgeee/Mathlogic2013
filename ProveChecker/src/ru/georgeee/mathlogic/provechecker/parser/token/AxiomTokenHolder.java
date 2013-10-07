package ru.georgeee.mathlogic.provechecker.parser.token;

import ru.georgeee.mathlogic.provechecker.expression.AxiomExpression;
import ru.georgeee.mathlogic.provechecker.expression.Expression;

import java.util.HashMap;

/**
 * Created with IntelliJ IDEA.
 * User: georgeee
 * Date: 07.10.13
 * Time: 4:55
 * To change this template use File | Settings | File Templates.
 */
public class AxiomTokenHolder extends TokenHolder {

    public final HashMap<String, Expression> varMapping = new HashMap<String, Expression>();


    @Override
    protected ManualTokenTypeFactory[] generateManualTokenTypeFabrics() {
        final ManualTokenTypeFactory[] _factories = super.generateManualTokenTypeFabrics();
        ManualTokenTypeFactory[] factories = new ManualTokenTypeFactory[_factories.length];
        for (int i = 0; i < factories.length; ++i) {
            final ManualTokenTypeFactory factory = _factories[i];
            factories[i] = new ManualTokenTypeFactory() {
                @Override
                public TokenType getTokenType(final String part) {
                    return new TokenType() {
                        @Override
                        public boolean isOpenningBracket() {
                            return parent.isOpenningBracket();  //To change body of implemented methods use File | Settings | File Templates.
                        }

                        @Override
                        public boolean isClosingBracket() {
                            return parent.isClosingBracket();  //To change body of implemented methods use File | Settings | File Templates.
                        }

                        TokenType parent = factory.getTokenType(part);

                        @Override
                        public Expression getExpression(Expression leftOperand, Expression rightOperand) {
                            return new AxiomExpression(parent.getExpression(leftOperand, rightOperand), varMapping);
                        }

                        @Override
                        public boolean isLeftOperandUsed() {
                            return parent.isLeftOperandUsed();  //To change body of implemented methods use File | Settings | File Templates.
                        }

                        @Override
                        public boolean isRightOperandUsed() {
                            return parent.isRightOperandUsed();  //To change body of implemented methods use File | Settings | File Templates.
                        }

                        @Override
                        public int getPriority() {
                            return parent.getPriority();  //To change body of implemented methods use File | Settings | File Templates.
                        }
                    };
                }
            };
        }
        return factories;
    }

    @Override
    protected AhoTokenType[] generateAhoTokenTypes() {
        AhoTokenType[] _ahoTokenTypes = super.generateAhoTokenTypes();
        AhoTokenType[] ahoTokenTypes = new AhoTokenType[_ahoTokenTypes.length];
        for (int i = 0; i < _ahoTokenTypes.length; ++i) {
            final AhoTokenType ahoTokenType = _ahoTokenTypes[i];
            ahoTokenTypes[i] = new AhoAbstractTokenType() {
                @Override
                public String getMatchString() {
                    return ahoTokenType.getMatchString();
                }

                @Override
                public Expression getExpression(Expression leftOperand, Expression rightOperand) {
                    return new AxiomExpression(ahoTokenType.getExpression(leftOperand, rightOperand), varMapping);
                }

                @Override
                public boolean isLeftOperandUsed() {
                    return ahoTokenType.isLeftOperandUsed();
                }

                @Override
                public boolean isRightOperandUsed() {
                    return ahoTokenType.isRightOperandUsed();
                }

                @Override
                public int getPriority() {
                    return ahoTokenType.getPriority();
                }

                @Override
                public boolean isOpenningBracket() {
                    return ahoTokenType.isOpenningBracket();
                }

                @Override
                public boolean isClosingBracket() {
                    return ahoTokenType.isClosingBracket();
                }
            };
        }
        return ahoTokenTypes;
    }
}
