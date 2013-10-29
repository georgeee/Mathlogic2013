package ru.georgeee.mathlogic.propositionalcalculus.parser.token;

import ru.georgeee.mathlogic.propositionalcalculus.ExpressionCompiler;
import ru.georgeee.mathlogic.propositionalcalculus.expression.Expression;
import ru.georgeee.mathlogic.propositionalcalculus.expression.StringConstants;
import ru.georgeee.mathlogic.propositionalcalculus.expression.Variable;
import ru.georgeee.mathlogic.propositionalcalculus.expression.operator.And;
import ru.georgeee.mathlogic.propositionalcalculus.expression.operator.Implication;
import ru.georgeee.mathlogic.propositionalcalculus.expression.operator.Not;
import ru.georgeee.mathlogic.propositionalcalculus.expression.operator.Or;

/**
 * Created with IntelliJ IDEA.
 * User: georgeee
 * Date: 05.10.13
 * Time: 6:34
 * To change this template use File | Settings | File Templates.
 */
public class TokenHolder {
    public static final int BRACKET_LEVEL = 0;
    public static final int VARIABLE_LEVEL = BRACKET_LEVEL + 1;
    public static final int NOT_LEVEL = VARIABLE_LEVEL + 1;
    public static final int AND_LEVEL = NOT_LEVEL + 1;
    public static final int OR_LEVEL = AND_LEVEL + 1;
    public static final int IMPLICATION_LEVEL = OR_LEVEL + 1;
    public static final boolean[] IS_RIGHT_ASSOC_LEVEL = new boolean[20];

    static {
        IS_RIGHT_ASSOC_LEVEL[NOT_LEVEL] = true;
        IS_RIGHT_ASSOC_LEVEL[IMPLICATION_LEVEL] = true;
    }

    ManualTokenTypeFactory[] manualTokenTypeFactories = null;
    AhoTokenType[] ahoTokenTypes = null;

    protected ExpressionCompiler expressionCompiler;

    public ExpressionCompiler getExpressionCompiler() {
        if (expressionCompiler == null) expressionCompiler = new ExpressionCompiler(this);
        return expressionCompiler;
    }

    public ManualTokenTypeFactory[] getManualTokenTypeFabrics() {
        if (manualTokenTypeFactories == null) manualTokenTypeFactories = generateManualTokenTypeFabrics();
        return manualTokenTypeFactories;
    }

    protected ManualTokenTypeFactory[] generateManualTokenTypeFabrics() {
        return new ManualTokenTypeFactory[]{
                new ManualTokenTypeFactory() {
                    @Override
                    public TokenType getTokenType(final String part) {
                        if (!part.matches("^[a-zA-Z0-9_α-ωΑ-ΣΤ-Ω]+$")) return null;
                        return new TokenType() {
                            @Override
                            public Expression getExpression(Expression leftOperand, Expression rightOperand) {
                                return new Variable(part);
                            }

                            @Override
                            public boolean isLeftOperandUsed() {
                                return false;
                            }

                            @Override
                            public boolean isRightOperandUsed() {
                                return false;
                            }

                            @Override
                            public int getPriority() {
                                return VARIABLE_LEVEL;
                            }

                            @Override
                            public boolean isOpenningBracket() {
                                return false;
                            }

                            @Override
                            public boolean isClosingBracket() {
                                return false;
                            }
                        };
                    }
                }
        };
    }

    public AhoTokenType[] getAhoTokenTypes() {
        if (ahoTokenTypes == null) ahoTokenTypes = generateAhoTokenTypes();
        return ahoTokenTypes;
    }

    protected AhoTokenType[] generateAhoTokenTypes() {
        return new AhoTokenType[]{
                new NotTokenType(),
                new AndTokenType(),
                new OrTokenType(),
                new ImplicationTokenType(),
                new NotTokenType(StringConstants.NOT_OPERATION_ALT),
                new AndTokenType(StringConstants.AND_OPERATION_ALT),
                new OrTokenType(StringConstants.OR_OPERATION_ALT),
                new ImplicationTokenType(StringConstants.IMPLICATION_OPERATION_ALT),
                new OpenningBracketTokenType(),
                new ClosingBracketTokenType(),
        };
    }

    static class AndTokenType extends AhoBothArgTokenType {
        @Override
        protected Expression getExpressionImpl(Expression leftOperand, Expression rightOperand) {
            return new And(leftOperand, rightOperand);
        }

        @Override
        public int getPriority() {
            return AND_LEVEL;
        }

        String matchString;

        AndTokenType(String matchString) {
            this.matchString = matchString;
        }

        AndTokenType() {
            this.matchString = StringConstants.AND_OPERATION;
        }

        @Override
        public String getMatchString() {
            return matchString;
        }
    }

    static class NotTokenType extends AhoRightArgTokenType {

        @Override
        protected Expression getExpressionImpl(Expression rightOperand) {
            return new Not(rightOperand);
        }

        @Override
        public int getPriority() {
            return NOT_LEVEL;
        }

        String matchString;

        NotTokenType(String matchString) {
            this.matchString = matchString;
        }

        NotTokenType() {
            this.matchString = StringConstants.NOT_OPERATION;
        }

        @Override
        public String getMatchString() {
            return matchString;
        }
    }

    static class OrTokenType extends AhoBothArgTokenType {
        @Override
        protected Expression getExpressionImpl(Expression leftOperand, Expression rightOperand) {
            return new Or(leftOperand, rightOperand);
        }

        @Override
        public int getPriority() {
            return OR_LEVEL;
        }

        String matchString;

        OrTokenType(String matchString) {
            this.matchString = matchString;
        }

        OrTokenType() {
            this.matchString = StringConstants.OR_OPERATION;
        }

        @Override
        public String getMatchString() {
            return matchString;
        }
    }

    static class ImplicationTokenType extends AhoBothArgTokenType {
        @Override
        protected Expression getExpressionImpl(Expression leftOperand, Expression rightOperand) {
            return new Implication(leftOperand, rightOperand);
        }

        @Override
        public int getPriority() {
            return IMPLICATION_LEVEL;
        }

        String matchString;

        ImplicationTokenType(String matchString) {
            this.matchString = matchString;
        }

        ImplicationTokenType() {
            this.matchString = StringConstants.IMPLICATION_OPERATION;
        }

        @Override
        public String getMatchString() {
            return matchString;
        }
    }

    abstract static class AhoAbstractTokenType implements AhoTokenType {
    }

    abstract static class AhoNoArgTokenType implements AhoTokenType {
        @Override
        public boolean isLeftOperandUsed() {
            return false;
        }

        @Override
        public boolean isRightOperandUsed() {
            return false;
        }
    }

    public static class OpenningBracketTokenType extends BracketTokenType {

        @Override
        public boolean isOpenning() {
            return true;
        }

        @Override
        public String getMatchString() {
            return "(";
        }
    }

    public static class ClosingBracketTokenType extends BracketTokenType {

        @Override
        public boolean isOpenning() {
            return false;
        }

        @Override
        public String getMatchString() {
            return ")";
        }
    }

    abstract static class BracketTokenType extends AhoNoArgTokenType {
        @Override
        public Expression getExpression(Expression leftOperand, Expression rightOperand) {
            return null;
        }

        @Override
        public int getPriority() {
            return BRACKET_LEVEL;
        }

        public abstract boolean isOpenning();

        @Override
        public boolean isOpenningBracket() {
            return isOpenning();
        }

        @Override
        public boolean isClosingBracket() {
            return !isOpenning();
        }
    }

    abstract static class AhoLeftArgTokenType extends LeftArgTokenType implements AhoTokenType {
    }

    abstract static class AhoRightArgTokenType extends RightArgTokenType implements AhoTokenType {
    }

    abstract static class AhoBothArgTokenType extends BothArgTokenType implements AhoTokenType {
    }

}
