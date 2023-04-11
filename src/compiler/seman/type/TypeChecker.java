/**
 * @ Author: turk
 * @ Description: Preverjanje tipov.
 */

package compiler.seman.type;

import static common.RequireNonNull.requireNonNull;

import common.Report;
import compiler.common.Visitor;
import compiler.parser.ast.def.*;
import compiler.parser.ast.def.FunDef.Parameter;
import compiler.parser.ast.expr.*;
import compiler.parser.ast.type.*;
import compiler.seman.common.NodeDescription;
import compiler.seman.type.type.Type;

import java.util.ArrayList;
import java.util.Optional;

public class TypeChecker implements Visitor {
    /**
     * Opis vozlišč in njihovih definicij.
     */
    private final NodeDescription<Def> definitions;

    /**
     * Opis vozlišč, ki jim priredimo podatkovne tipe.
     */
    private NodeDescription<Type> types;

    public TypeChecker(NodeDescription<Def> definitions, NodeDescription<Type> types) {
        requireNonNull(definitions, types);
        this.definitions = definitions;
        this.types = types;
    }

    @Override
    public void visit(Call call) {
        // get definition and function type
        Optional<Def> def = this.definitions.valueFor(call);
        def.get().accept(this);
        Optional<Type> functionType = this.types.valueFor(def.get());
        // check if definition exists
        if (functionType.isEmpty())
            Report.error(call.position, "No type found for call at position: " + call.position);

        // check if it's a function call
        if (!(functionType.get() instanceof Type.Function))
            Report.error(call.position, "Variable or type can't be called. Called at position: " + call.position);
        Type.Function function = functionType.get().asFunction().get();

        // get parameter and argument counts
        int parameterCount = functionType.get().asFunction().get().parameters.size();
        int argumentCount = call.arguments.size();
        if (parameterCount != argumentCount)
            Report.error(call.position, "Number of arguments doesn't match number of parameters. Called at position: " + call.position + "\nExpected " + parameterCount + " arguments, got " + argumentCount);

        // check if types match for all arguments
        for (int i = 0; i < parameterCount; i++) {
            Expr argument = call.arguments.get(i);
            Type parameterType = function.parameters.get(i);
            argument.accept(this);
            Optional<Type> argumentType = this.types.valueFor(argument);
            if (argumentType.isEmpty())
                Report.error(argument.position, "No type found for argument at position: " + argument.position);
            if (!argumentType.get().equals(parameterType))
                Report.error(argument.position, "Argument type doesn't match parameter type. Called at position: " + argument.position + "\nExpected " + parameterType + ", got " + argumentType.get());
        }

        this.types.store(function.returnType, call);
    }

    @Override
    public void visit(Binary binary) {
        // get types
        binary.left.accept(this);
        binary.right.accept(this);
        Optional<Type> leftType = this.types.valueFor(binary.left);
        Optional<Type> rightType = this.types.valueFor(binary.right);

        // check for types
        if (leftType.isEmpty() || rightType.isEmpty())
            Report.error(binary.position, "No type found for binary expression at position: " + binary.position);

        // check for array call
        if (binary.operator == Binary.Operator.ARR) {
            if (!(leftType.get() instanceof Type.Array))
                Report.error(binary.left.position, "Array call on non-array type at position: " + binary.left.position);
            if (!rightType.get().isInt())
                Report.error(binary.right.position, "Index must be of integer type at position: " + binary.right.position);
            Type arrayType = leftType.get().asArray().get().type;
            this.types.store(arrayType, binary);
            return;
        }

        // check if types are equal
        if (!leftType.get().equals(rightType.get()))
            Report.error(binary.position, "Binary expression at position: " + binary.position + " has different types on both sides");

        // check for assignment
        if (binary.operator == Binary.Operator.ASSIGN) {
            if (!(leftType.get() instanceof Type.Atom))
                Report.error(binary.left.position, "Assignment can only be used on atom types at position: " + binary.left.position);
            this.types.store(leftType.get(), binary);
            return;
        }

        // check for and or
        if (binary.operator.isAndOr()) {
            if (!leftType.get().isLog())
                Report.error(binary.position, "And and or operators can only be used on boolean types at position: " + binary.position);
            Type.Atom log = new Type.Atom(Type.Atom.Kind.LOG);
            this.types.store(log, binary);
            return;
        }

        // check for arithmetic
        if (binary.operator.isArithmetic()) {
            if (!leftType.get().isInt())
                Report.error(binary.position, "Arithmetic operators can only be used on integer types at position: " + binary.position);
            Type.Atom intType = new Type.Atom(Type.Atom.Kind.INT);
            this.types.store(intType, binary);
            return;
        }

        // check for comparison
        if (binary.operator.isComparison()) {
            if (!(leftType.get().isInt() || leftType.get().isLog()))
                Report.error(binary.position, "Comparison operators can only be used on integer and boolean types at position: " + binary.position);
            Type.Atom log = new Type.Atom(Type.Atom.Kind.LOG);
            this.types.store(log, binary);
            return;
        }

        // curently unknown error
        Report.error(binary.position, "Something wrong in binary expression at position: " + binary.position);
    }

    @Override
    public void visit(Block block) {
        // visit all expressions
        for (Expr expr: block.expressions) {
            expr.accept(this);
        }
        // make the block type, the type of the last expression
        this.types.store(this.types.valueFor(block.expressions.get(block.expressions.size() - 1)).get(), block);
    }

    @Override
    public void visit(For forLoop) {
        // visit all expressions and get types
        forLoop.counter.accept(this);
        forLoop.low.accept(this);
        forLoop.high.accept(this);
        forLoop.step.accept(this);
        forLoop.body.accept(this);
        Optional<Type> counterType = this.types.valueFor(forLoop.counter);
        Optional<Type> lowType = this.types.valueFor(forLoop.low);
        Optional<Type> highType = this.types.valueFor(forLoop.high);
        Optional<Type> stepType = this.types.valueFor(forLoop.step);
        Optional<Type> bodyType = this.types.valueFor(forLoop.body);

        // check if types exist
        if (counterType.isEmpty() || lowType.isEmpty() || highType.isEmpty() || stepType.isEmpty() || bodyType.isEmpty())
            Report.error(forLoop.position, "No type found for for loop at position: " + forLoop.position);

        // check if types are correct
        if (!counterType.get().isInt())
            Report.error(forLoop.counter.position, "Counter must be of integer type at position: " + forLoop.counter.position);
        if (!lowType.get().isInt())
            Report.error(forLoop.low.position, "Low must be of integer type at position: " + forLoop.low.position);
        if (!highType.get().isInt())
            Report.error(forLoop.high.position, "High must be of integer type at position: " + forLoop.high.position);
        if (!stepType.get().isInt())
            Report.error(forLoop.step.position, "Step must be of integer type at position: " + forLoop.step.position);

        // save void type for for-loop
        Type.Atom voidType = new Type.Atom(Type.Atom.Kind.VOID);
        this.types.store(voidType, forLoop);
    }

    @Override
    public void visit(Name name) {
        Optional<Def> def = this.definitions.valueFor(name);
        Optional<Type> type = this.types.valueFor(def.get());
        if (type.isEmpty())
            Report.error(name.position, "No type found for name: '" + name.name + "' at position: " + name.position);
        this.types.store(type.get(), name);
    }

    @Override
    public void visit(IfThenElse ifThenElse) {
        // visit expression types
        ifThenElse.condition.accept(this);
        ifThenElse.thenExpression.accept(this);
        if (ifThenElse.elseExpression.isPresent())
            ifThenElse.elseExpression.get().accept(this);

        // get expression types
        Optional<Type> conditionType = this.types.valueFor(ifThenElse.condition);

        // check for LOG type
        if (!conditionType.get().isLog())
            Report.error(ifThenElse.condition.position, "Condition must be of logical type at position: " + ifThenElse.condition.position);

        // save void type
        Type.Atom voidType = new Type.Atom(Type.Atom.Kind.VOID);
        this.types.store(voidType, ifThenElse);
    }

    @Override
    public void visit(Literal literal) {
        Type.Atom.Kind kind = switch (literal.type) {
            case STR -> Type.Atom.Kind.STR;
            case INT -> Type.Atom.Kind.INT;
            case LOG -> Type.Atom.Kind.LOG;
        };
        Type.Atom atom = new Type.Atom(kind);
        this.types.store(atom, literal);
    }

    @Override
    public void visit(Unary unary) {
        // visit and expression type
        unary.expr.accept(this);
        Optional<Type> type = this.types.valueFor(unary.expr);
        if (type.isEmpty())
            Report.error(unary.position, "No type found for unary expression at position: " + unary.position);

        // check for correct type for operator
        if (unary.operator == Unary.Operator.NOT) {
            if (!type.get().isLog())
                Report.error(unary.position, "Unary expression at position: " + unary.position + " must be of logical type");
        } else if (unary.operator == Unary.Operator.ADD || unary.operator == Unary.Operator.SUB) {
            if (!type.get().isInt())
                Report.error(unary.position, "Unary expression at position: " + unary.position + " must be of integer type");
        }

        // store type
        this.types.store(type.get(), unary);
    }

    @Override
    public void visit(While whileLoop) {
        // visit all expressions and get types
        whileLoop.condition.accept(this);
        whileLoop.body.accept(this);

        // get and check condition type
        Optional<Type> conditionType = this.types.valueFor(whileLoop.condition);
        if (!conditionType.get().isLog())
            Report.error(whileLoop.condition.position, "Condition must be of logical type at position: " + whileLoop.condition.position);

        // save void type
        Type.Atom voidType = new Type.Atom(Type.Atom.Kind.VOID);
        this.types.store(voidType, whileLoop);
    }

    @Override
    public void visit(Where where) {
        // accept defs
        where.defs.accept(this);
        // get expression type
        where.expr.accept(this);
        Optional<Type> type = this.types.valueFor(where.expr);
        this.types.store(type.get(), where);
    }

    @Override
    public void visit(Defs defs) {
        for (Def def:defs.definitions) {
            def.accept(this);
        }
    }

    @Override
    public void visit(FunDef funDef) {
        // check if already defined
        Optional<Type> type = this.types.valueFor(funDef);
        if (type.isPresent())
            return;

        // get parameter types
        ArrayList<Type> parameters = new ArrayList<>();

        for (Parameter par:funDef.parameters) {
            par.accept(this);
            type = this.types.valueFor(par);
            parameters.add(type.get());
        }

        // get return type
        Type returnType;
        funDef.type.accept(this);
        type = this.types.valueFor(funDef.type);
        returnType = type.get();

        // save
        Type.Function funType = new Type.Function(parameters, returnType);
        this.types.store(funType, funDef);

        // visit body
        funDef.body.accept(this);
    }

    @Override
    public void visit(TypeDef typeDef) {
        typeDef.type.accept(this);
        Optional<Type> type = this.types.valueFor(typeDef.type);
        this.types.store(type.get(), typeDef);
    }

    @Override
    public void visit(VarDef varDef) {
        varDef.type.accept(this);
        Optional<Type> type = this.types.valueFor(varDef.type);
        this.types.store(type.get(), varDef);
    }

    @Override
    public void visit(Parameter parameter) {
        parameter.type.accept(this);
        Optional<Type> type = this.types.valueFor(parameter.type);
        this.types.store(type.get(), parameter);
    }

    @Override
    public void visit(Array array) {
        int size = array.size;
        array.type.accept(this);
        Optional<Type> type = this.types.valueFor(array.type);
        Type.Array arrayType = new Type.Array(size, type.get());
        this.types.store(arrayType, array);
    }

    @Override
    public void visit(Atom atom) {
        Type.Atom.Kind kind = switch (atom.type) {
            case LOG -> Type.Atom.Kind.LOG;
            case INT -> Type.Atom.Kind.INT;
            case STR -> Type.Atom.Kind.STR;
        };
        Type.Atom atomType = new Type.Atom(kind);
        this.types.store(atomType, atom);
    }

    @Override
    public void visit(TypeName name) {
        Optional<Def> def = this.definitions.valueFor(name);
        if (def.isEmpty()) {
            Report.error(name.position, "Missing definition for: " + name.identifier + ", at: " + name.position);
        }

        def.get().accept(this);
        Optional<Type> type = this.types.valueFor(def.get());
        if (type.isEmpty()) {
            Report.error(def.get().position, "Something wrong in visitor TypeName at: " + def.get().position);
        }

        this.types.store(type.get(), name);
    }
}
