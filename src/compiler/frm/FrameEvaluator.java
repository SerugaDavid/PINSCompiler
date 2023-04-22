/**
 * @ Author: turk
 * @ Description: Analizator klicnih zapisov.
 */

package compiler.frm;

import static common.RequireNonNull.requireNonNull;

import compiler.common.Visitor;
import compiler.parser.ast.def.*;
import compiler.parser.ast.def.FunDef.Parameter;
import compiler.parser.ast.expr.*;
import compiler.parser.ast.type.Array;
import compiler.parser.ast.type.Atom;
import compiler.parser.ast.type.TypeName;
import compiler.seman.common.NodeDescription;
import compiler.seman.type.type.Type;

public class FrameEvaluator implements Visitor {
    /**
     * Opis definicij funkcij in njihovih klicnih zapisov.
     */
    private NodeDescription<Frame> frames;

    /**
     * Opis definicij spremenljivk in njihovih dostopov.
     */
    private NodeDescription<Access> accesses;

    /**
     * Opis vozlišč in njihovih definicij.
     */
    private final NodeDescription<Def> definitions;

    /**
     * Opis vozlišč in njihovih podatkovnih tipov.
     */
    private final NodeDescription<Type> types;

    /**
     * Ali smo trenutno v globalnem okolju.
     */
    private boolean isGlobal;

    /**
     * Klicni zapis trenutne funkcije.
     */
    private Frame.Builder frameBuilder;

    public FrameEvaluator(
        NodeDescription<Frame> frames, 
        NodeDescription<Access> accesses,
        NodeDescription<Def> definitions,
        NodeDescription<Type> types
    ) {
        requireNonNull(frames, accesses, definitions, types);
        this.frames = frames;
        this.accesses = accesses;
        this.definitions = definitions;
        this.types = types;
        this.isGlobal = true;
        this.frameBuilder = null;
    }

    @Override
    public void visit(Call call) {
        for (Expr expr : call.arguments)
            expr.accept(this);
        this.frameBuilder.addFunctionCall(call.arguments.size() + 1);
    }


    @Override
    public void visit(Binary binary) {
        binary.left.accept(this);
        binary.right.accept(this);
    }


    @Override
    public void visit(Block block) {
        for (Expr expr : block.expressions)
            expr.accept(this);
    }


    @Override
    public void visit(For forLoop) {
        forLoop.low.accept(this);
        forLoop.high.accept(this);
        forLoop.step.accept(this);
        forLoop.body.accept(this);
    }


    @Override
    public void visit(Name name) {
        // This method is not needed.
        throw new UnsupportedOperationException("Unimplemented method 'visit'");
    }


    @Override
    public void visit(IfThenElse ifThenElse) {
        ifThenElse.condition.accept(this);
        ifThenElse.thenExpression.accept(this);
        ifThenElse.elseExpression.get().accept(this);
    }


    @Override
    public void visit(Literal literal) {
        // This method is not needed.
        throw new UnsupportedOperationException("Unimplemented method 'visit'");
    }


    @Override
    public void visit(Unary unary) {
        unary.expr.accept(this);
    }


    @Override
    public void visit(While whileLoop) {
        whileLoop.condition.accept(this);
        whileLoop.body.accept(this);
    }


    @Override
    public void visit(Where where) {
        where.defs.accept(this);
        where.expr.accept(this);
    }


    @Override
    public void visit(Defs defs) {
        boolean isGlobal = this.isGlobal;
        for (Def def : defs.definitions) {
            def.accept(this);
            this.isGlobal = isGlobal;
        }
    }


    @Override
    public void visit(FunDef funDef) {
        // save previous frame builder
        Frame.Builder prev = this.frameBuilder;

        // create new frame builder
        if (this.isGlobal) {
            this.isGlobal = false;
            this.frameBuilder = new Frame.Builder(Frame.Label.named(funDef.name), 1);
        } else
            this.frameBuilder = new Frame.Builder(Frame.Label.nextAnonymous(), prev.staticLevel + 1);

        // visit parameters
        for (Parameter parameter : funDef.parameters)
            parameter.accept(this);

        // visit body
        funDef.body.accept(this);

        // save and restore frame
        this.frames.store(this.frameBuilder.build(), funDef);
        this.frameBuilder = prev;
    }


    @Override
    public void visit(TypeDef typeDef) {
        // This method is not needed.
        throw new UnsupportedOperationException("Unimplemented method 'visit'");
    }


    @Override
    public void visit(VarDef varDef) {
        Access access;
        if (this.isGlobal) {
            access = new Access.Global(this.types.valueFor(varDef).get().sizeInBytes(), Frame.Label.named(varDef.name));
        } else {
            int size = this.types.valueFor(varDef).get().sizeInBytes();
            int offset = this.frameBuilder.addLocalVariable(size);
            int staticLevel = this.frameBuilder.staticLevel;
            access = new Access.Local(this.types.valueFor(varDef).get().sizeInBytes(), offset, staticLevel);
        }
        this.accesses.store(access, varDef);
    }


    @Override
    public void visit(Parameter parameter) {
        int size = this.types.valueFor(parameter).get().sizeInBytes();
        int offset = this.frameBuilder.addParameter(size);
        int staticLevel = this.frameBuilder.staticLevel;
        Access access = new Access.Local(size, offset, staticLevel);
        this.accesses.store(access, parameter);
    }


    @Override
    public void visit(Array array) {
        // This method is not needed.
        throw new UnsupportedOperationException("Unimplemented method 'visit'");
    }


    @Override
    public void visit(Atom atom) {
        // This method is not needed.
        throw new UnsupportedOperationException("Unimplemented method 'visit'");
    }


    @Override
    public void visit(TypeName name) {
        // This method is not needed.
        throw new UnsupportedOperationException("Unimplemented method 'visit'");
    }
}
