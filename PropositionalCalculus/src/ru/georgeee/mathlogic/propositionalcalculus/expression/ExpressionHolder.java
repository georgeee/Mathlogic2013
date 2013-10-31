package ru.georgeee.mathlogic.propositionalcalculus.expression;

import ru.georgeee.mathlogic.propositionalcalculus.expression.operator.And;
import ru.georgeee.mathlogic.propositionalcalculus.expression.operator.Implication;
import ru.georgeee.mathlogic.propositionalcalculus.expression.operator.Not;
import ru.georgeee.mathlogic.propositionalcalculus.expression.operator.Or;

import java.util.HashMap;
import java.util.HashSet;
import java.util.WeakHashMap;

/**
 * Created with IntelliJ IDEA.
 * User: georgeee
 * Date: 31.10.13
 * Time: 7:43
 * To change this template use File | Settings | File Templates.
 */
public class ExpressionHolder {
    private ExpressionHolder(){}
    private static ExpressionHolder _instance = null;
    public static ExpressionHolder instance(){
        return _instance==null?(_instance = new ExpressionHolder()): _instance;
    }

    WeakHashMap<Expression, Expression> expressions = new WeakHashMap<Expression, Expression>();

    public And getAndExpression(Expression leftOperand, Expression rightOperand){
        And newObject = new And(leftOperand, rightOperand);
        And oldObject = (And) expressions.get(newObject);
        if(oldObject != null) return oldObject;
        else expressions.put(newObject, newObject);
        return newObject;
    }
    public Not getNotExpression(Expression operand){
        Not newObject = new Not(operand);
        Not oldObject = (Not) expressions.get(newObject);
        if(oldObject != null) return oldObject;
        else expressions.put(newObject, newObject);
        return newObject;
    }
    public Variable getVariableExpression(String varName){
        Variable newObject = new Variable(varName);
        Variable oldObject = (Variable) expressions.get(newObject);
        if(oldObject != null) return oldObject;
        else expressions.put(newObject, newObject);
        return newObject;
    }

    public Implication getImplicationExpression(Expression leftOperand, Expression rightOperand){
        Implication newObject = new Implication(leftOperand, rightOperand);
        Implication oldObject = (Implication) expressions.get(newObject);
        if(oldObject != null) return oldObject;
        else expressions.put(newObject, newObject);
        return newObject;
    }
    public Or getOrExpression(Expression leftOperand, Expression rightOperand){
        Or newObject = new Or(leftOperand, rightOperand);
        Or oldObject = (Or) expressions.get(newObject);
        if(oldObject != null) return oldObject;
        else expressions.put(newObject, newObject);
        return newObject;
    }
}
