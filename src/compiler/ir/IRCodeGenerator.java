/**
 * @ Author: turk
 * @ Description: Generator vmesne kode.
 */

package compiler.ir;

import static common.RequireNonNull.requireNonNull;

import java.util.ArrayList;
import java.util.Arrays;
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
     * Trenutni frame
     */
    private Frame currentFrame;

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
        // get oldFpPosition
        ConstantExpr offset = new ConstantExpr(this.currentFrame.oldFPOffset());
        BinopExpr pointer = new BinopExpr(NameExpr.SP(), offset, BinopExpr.Operator.SUB);
        MemExpr value = new MemExpr(pointer);

        // move fp to oldFp position and create call
        MoveStmt move = new MoveStmt(value, NameExpr.FP());
        List<IRExpr> args = new ArrayList<>();
        args.add(/* TODO: Static link */null);
        for (Expr arg : call.arguments) {
            arg.accept(this);
            args.add((IRExpr) this.imcCode.valueFor(arg).get());
        }
        CallExpr functionCall = new CallExpr(/* TODO: moramo dobit en label */null, args);

        // join moving and calling
        EseqExpr fullCall = new EseqExpr(move, functionCall);
    }

    @Override
    public void visit(Binary binary) {
        // get nodes
        binary.left.accept(this);
        binary.right.accept(this);
        IRNode left = this.imcCode.valueFor(binary.left).get();
        IRNode right = this.imcCode.valueFor(binary.right).get();

        // check operators and store
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
        // labels
        Frame.Label loopStart = Frame.Label.nextAnonymous();
        Frame.Label loopEnd = Frame.Label.nextAnonymous();
        Frame.Label loopBody = Frame.Label.nextAnonymous();
        LabelStmt loopStartStmt = new LabelStmt(loopStart);
        LabelStmt loopEndStmt = new LabelStmt(loopEnd);
        LabelStmt loopBodyStmt = new LabelStmt(loopBody);

        // loop logic
        forLoop.counter.accept(this);
        forLoop.low.accept(this);
        forLoop.high.accept(this);
        forLoop.step.accept(this);
        IRExpr counter = (IRExpr) this.imcCode.valueFor(forLoop.counter).get();
        IRExpr low = (IRExpr) this.imcCode.valueFor(forLoop.low).get();
        IRExpr high = (IRExpr) this.imcCode.valueFor(forLoop.high).get();
        IRExpr step = (IRExpr) this.imcCode.valueFor(forLoop.step).get();

        MoveStmt init = new MoveStmt(counter, low);
        BinopExpr addition = new BinopExpr(counter, step, BinopExpr.Operator.ADD);
        MoveStmt increment = new MoveStmt(counter, addition);
        BinopExpr condition = new BinopExpr(counter, high, BinopExpr.Operator.LT);
        CJumpStmt cjump = new CJumpStmt(condition, loopStart, loopEnd);
        JumpStmt jump = new JumpStmt(loopBody);

        // assemble with body
        forLoop.body.accept(this);
        IRNode body = this.imcCode.valueFor(forLoop.body).get();
        if (body instanceof IRExpr)
            body = new ExpStmt((IRExpr) body);
        List<IRStmt> statements = Arrays.asList(init, loopStartStmt, cjump, loopBodyStmt, (IRStmt) body, increment, jump, loopEndStmt);

        // create SeqStmt
        SeqStmt seq = new SeqStmt(statements);
        this.imcCode.store(seq, forLoop);
    }

    @Override
    public void visit(Name name) {
        // TODO: tle moraš access gledat da pogruntaš al je globalno al je lokalno

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
        Atom.Type literalType = literal.type;
        ConstantExpr value = switch (literalType) {
            case INT -> new ConstantExpr(Integer.parseInt(literal.value));
            case LOG -> new ConstantExpr(literal.value.equals("true") ? 1 : 0);
            case STR -> {
                Access.Global globalAcces = new Access.Global(
                        literal.value.length() * Constants.WordSize,
                        Label.nextAnonymous()
                );
                Chunk.DataChunk string = new Chunk.DataChunk(globalAcces, literal.value);
                // tle moraš not NAME dat
                yield new ConstantExpr(/* TODO: tle not mora bit nek pointer */0);
            }
        };
        this.imcCode.store(value, literal);
    }

    @Override
    public void visit(Unary unary) {
        unary.expr.accept(this);
        IRExpr expr = (IRExpr) this.imcCode.valueFor(unary.expr).get();
        ConstantExpr zero = new ConstantExpr(0);
        BinopExpr unop = switch (unary.operator) {
            case ADD -> new BinopExpr(zero, expr, BinopExpr.Operator.ADD);
            case SUB -> new BinopExpr(zero, expr, BinopExpr.Operator.SUB);
            case NOT -> new BinopExpr(new ConstantExpr(1), expr, BinopExpr.Operator.SUB);
        };
        this.imcCode.store(unop, unary);
    }

    @Override
    public void visit(While whileLoop) {
        // labels
        Frame.Label loopStart = Frame.Label.nextAnonymous();
        Frame.Label loopEnd = Frame.Label.nextAnonymous();
        Frame.Label loopBody = Frame.Label.nextAnonymous();
        LabelStmt loopStartStmt = new LabelStmt(loopStart);
        LabelStmt loopEndStmt = new LabelStmt(loopEnd);
        LabelStmt loopBodyStmt = new LabelStmt(loopBody);

        // loop logic
        whileLoop.condition.accept(this);
        IRExpr condition = (IRExpr) this.imcCode.valueFor(whileLoop.condition).get();

        CJumpStmt cjump = new CJumpStmt(condition, loopStart, loopEnd);
        JumpStmt jump = new JumpStmt(loopBody);

        // assemble with body
        whileLoop.body.accept(this);
        IRNode body = this.imcCode.valueFor(whileLoop.body).get();
        if (body instanceof IRExpr)
            body = new ExpStmt((IRExpr) body);
        List<IRStmt> statements = Arrays.asList(loopStartStmt, cjump, loopBodyStmt, (IRStmt) body, jump, loopEndStmt);

        // create SeqStmt
        SeqStmt seq = new SeqStmt(statements);
        this.imcCode.store(seq, whileLoop);
    }

    @Override
    public void visit(Where where) {
        where.defs.accept(this);
        where.expr.accept(this);
        IRNode value = this.imcCode.valueFor(where.expr).get();
        this.imcCode.store(value, where);
    }

    @Override
    public void visit(Defs defs) {
        for (Def def : defs.definitions) {
            def.accept(this);
        }
    }

    @Override
    public void visit(FunDef funDef) {
        // switch frame
        Frame prev = this.currentFrame;
        this.currentFrame = this.frames.valueFor(funDef).get();

        // visit body; TODO: A rabimo sploh kej delat z argumenti
        funDef.body.accept(this);
        IRNode body = this.imcCode.valueFor(funDef.body).get();

        // move(return value placement, koda funckije)
        IRExpr returnValue;
        if (body instanceof IRExpr)
            returnValue = (IRExpr) body;
        else
            returnValue = new EseqExpr((IRStmt) body, new ConstantExpr(0));

        MemExpr location = new MemExpr(NameExpr.FP());
        MoveStmt saveReturn = new MoveStmt(location, returnValue);

        // create code chunk
        Chunk.CodeChunk code = new Chunk.CodeChunk(this.currentFrame, saveReturn);
        this.chunks.add(code);

        // reset frame
        this.currentFrame = prev;
    }

    @Override
    public void visit(TypeDef typeDef) {
        // this method is not needed
    }

    @Override
    public void visit(VarDef varDef) {
        Access access = this.accesses.valueFor(varDef).get();
        if (access instanceof Access.Global) {
            Chunk.GlobalChunk globalChunk = new Chunk.GlobalChunk((Access.Global) access);
            this.chunks.add(globalChunk);
        }
    }

    @Override
    public void visit(Parameter parameter) {
        // this method is not needed
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
