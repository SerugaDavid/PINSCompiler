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
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'visit'");
    }

    @Override
    public void visit(Binary binary) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'visit'");
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
