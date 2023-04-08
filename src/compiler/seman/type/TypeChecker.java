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

        // check for logical operators
        if (binary.operator.isAndOr() || binary.operator.isComparison()) {
            Type.Atom.Kind kind = Type.Atom.Kind.LOG;
            Type.Atom type = new Type.Atom(kind);
            this.types.store(type, binary);
            return;
        }

        // check for arithmetic operators
        if (binary.operator.isArithmetic()) {
            Type type = leftType.get();
            this.types.store(type, binary);
            return;
        }

        // curently unknown error
        Report.error(binary.position, "Something wrong in binary expression at position: " + binary.position);
    }

    @Override
    public void visit(Block block) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'visit'");
    }

    @Override
    public void visit(For forLoop) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'visit'");
    }

    @Override
    public void visit(Name name) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'visit'");
    }

    @Override
    public void visit(IfThenElse ifThenElse) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'visit'");
    }

    @Override
    public void visit(Literal literal) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'visit'");
    }

    @Override
    public void visit(Unary unary) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'visit'");
    }

    @Override
    public void visit(While whileLoop) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'visit'");
    }

    @Override
    public void visit(Where where) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'visit'");
    }

    @Override
    public void visit(Defs defs) {
        for (Def def:defs.definitions) {
            def.accept(this);
        }
    }

    @Override
    public void visit(FunDef funDef) {
        // get parameter types
        ArrayList<Type> parameters = new ArrayList<>();
        Optional<Type> type;
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
