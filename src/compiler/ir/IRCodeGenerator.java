/**
 * @ Author: turk
 * @ Description: Generator vmesne kode.
 */

package compiler.ir;

import static common.RequireNonNull.requireNonNull;

import java.util.ArrayList;
import java.util.List;

import common.Constants;
import compiler.common.Visitor;
import compiler.frm.Access;
import compiler.frm.Frame;
import compiler.frm.Frame.Label;
import compiler.ir.chunk.Chunk;
import compiler.ir.code.IRNode;
import compiler.ir.code.expr.*;
import compiler.ir.code.stmt.*;
import compiler.parser.ast.def.*;
import compiler.parser.ast.def.FunDef.Parameter;
import compiler.parser.ast.expr.*;
import compiler.parser.ast.type.Array;
import compiler.parser.ast.type.Atom;
import compiler.parser.ast.type.TypeName;
import compiler.seman.common.NodeDescription;
import compiler.seman.type.type.Type;

public class IRCodeGenerator implements Visitor {
    /**
     * Preslikava iz vozlišč AST v vmesno kodo.
     */
    private NodeDescription<IRNode> imcCode;

    /**
     * Razrešeni klicni zapisi.
     */
    private final NodeDescription<Frame> frames;

    /**
     * Razrešeni dostopi.
     */
    private final NodeDescription<Access> accesses;

    /**
     * Razrešene definicije.
     */
    private final NodeDescription<Def> definitions;

    /**
     * Razrešeni tipi.
     */
    private final NodeDescription<Type> types;

    /**
     * **Rezultat generiranja vmesne kode** - seznam fragmentov.
     */
    public List<Chunk> chunks = new ArrayList<>();

    public IRCodeGenerator(
        NodeDescription<IRNode> imcCode,
        NodeDescription<Frame> frames, 
        NodeDescription<Access> accesses,
        NodeDescription<Def> definitions,
        NodeDescription<Type> types
    ) {
        requireNonNull(imcCode, frames, accesses, definitions, types);
        this.types = types;
        this.imcCode = imcCode;
        this.frames = frames;
        this.accesses = accesses;
        this.definitions = definitions;
    }

    @Override
    public void visit(Call call) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'visit'");
    }

    @Override
    public void visit(Binary binary) {
        binary.left.accept(this);
        binary.right.accept(this);
        IRNode left = this.imcCode.valueFor(binary.left).get();
        IRNode right = this.imcCode.valueFor(binary.right).get();
        if (binary.operator == Binary.Operator.ASSIGN) {
            MoveStmt move = new MoveStmt((IRExpr) left, (IRExpr) right);
            this.imcCode.store(move, binary);
        } else if (binary.operator == Binary.Operator.ARR) {
            Label arrayLabel = this.frames.valueFor(binary.left).get().label;
            NameExpr nameExpr = new NameExpr(arrayLabel);
            BinopExpr pointer = new BinopExpr(nameExpr, (IRExpr) right, BinopExpr.Operator.ADD);
            MemExpr value = new MemExpr(pointer);
            this.imcCode.store(value, binary);
        } else {
            BinopExpr binop = new BinopExpr((IRExpr) left, (IRExpr) right, BinopExpr.convertOp(binary.operator));
            this.imcCode.store(binop, binary);
        }
    }

    @Override
    public void visit(Block block) {
        IRNode tmp = null;
        List<IRStmt> statements = new ArrayList<>();
        for (Expr expr : block.expressions) {
            expr.accept(this);
            tmp = this.imcCode.valueFor(expr).get();
            if (tmp instanceof IRExpr)
                tmp = new ExpStmt((IRExpr) tmp);
            statements.add((IRStmt) tmp);
        }
        SeqStmt seq = new SeqStmt(statements);
        this.imcCode.store(seq, block);
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
        // labels
        Frame.Label trueLabel = Frame.Label.nextAnonymous();
        Frame.Label falseLabel = Frame.Label.nextAnonymous();
        LabelStmt trueLabelStmt = new LabelStmt(trueLabel);
        LabelStmt falseLabelStmt = new LabelStmt(falseLabel);

        // condition
        ifThenElse.condition.accept(this);
        IRExpr condition = (IRExpr) this.imcCode.valueFor(ifThenElse.condition).get();
        CJumpStmt cjump = new CJumpStmt(condition, trueLabel, falseLabel);

        // assemble with body
        List<IRStmt> statements = new ArrayList<>();
        IRNode tmp;
        statements.add(cjump);
        statements.add(trueLabelStmt);
        ifThenElse.thenExpression.accept(this);
        tmp = this.imcCode.valueFor(ifThenElse.thenExpression).get();
        if (tmp instanceof IRExpr)
            tmp = new ExpStmt((IRExpr) tmp);
        statements.add((IRStmt) tmp);
        statements.add(falseLabelStmt);
        if (ifThenElse.elseExpression.isPresent()) {
            ifThenElse.elseExpression.get().accept(this);
            tmp = this.imcCode.valueFor(ifThenElse.elseExpression.get()).get();
            if (tmp instanceof IRExpr)
                tmp = new ExpStmt((IRExpr) tmp);
            statements.add((IRStmt) tmp);
        }

        // create SeqStmt
        SeqStmt seq = new SeqStmt(statements);
        this.imcCode.store(seq, ifThenElse);
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
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'visit'");
    }

    @Override
    public void visit(FunDef funDef) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'visit'");
    }

    @Override
    public void visit(TypeDef typeDef) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'visit'");
    }

    @Override
    public void visit(VarDef varDef) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'visit'");
    }

    @Override
    public void visit(Parameter parameter) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'visit'");
    }

    @Override
    public void visit(Array array) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'visit'");
    }

    @Override
    public void visit(Atom atom) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'visit'");
    }

    @Override
    public void visit(TypeName name) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'visit'");
    }
}
