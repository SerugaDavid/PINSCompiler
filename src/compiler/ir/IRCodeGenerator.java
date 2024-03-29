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
        Frame frame;
        // check stdlib
        // get frame
        if (this.definitions.valueFor(call).isPresent()) {
            Def definition = this.definitions.valueFor(call).get();
            frame = this.frames.valueFor(definition).get();
        } else {
            Frame.Builder builder = new Frame.Builder(Label.named(call.name), 1);
            for (Expr arg : call.arguments) {
                builder.addParameter(Constants.WordSize);
            }
            frame = builder.build();
        }


        // get oldFpPosition
        ConstantExpr offset = new ConstantExpr(frame.oldFPOffset());
        BinopExpr pointer = new BinopExpr(NameExpr.SP(), offset, BinopExpr.Operator.SUB);
        MemExpr value = new MemExpr(pointer);

        // move fp to oldFp position
        MoveStmt move = new MoveStmt(value, NameExpr.FP());

        // get static link
        int staticLevelCall = frame.staticLevel - 1; // TODO: is this correct?
        int staticLevelCurrent = this.currentFrame.staticLevel;
        int staticLevelDifference = staticLevelCurrent - staticLevelCall;
        IRExpr staticLink = NameExpr.FP();
        for (int i = 0; i < staticLevelDifference; i++) {
            staticLink = new MemExpr(staticLink);
        }

        // get arguments
        List<IRExpr> args = new ArrayList<>();
        if (staticLevelCall == 0)
            args.add(new ConstantExpr(0));
        else
            args.add(staticLink);
        for (Expr arg : call.arguments) {
            arg.accept(this);
            IRNode argNode = this.imcCode.valueFor(arg).get();
            if (arg instanceof Binary binary) {
                Type argType = this.types.valueFor(arg).get();
                if (binary.operator == Binary.Operator.ARR && !(argType instanceof Type.Array))
                    argNode = new MemExpr((IRExpr) argNode);
            }
            args.add((IRExpr) argNode);
        }

        // create call
        Label functionLabel = frame.label;
        CallExpr functionCall = new CallExpr(functionLabel, args);

        // join moving and calling
        EseqExpr fullCall = new EseqExpr(move, functionCall);

        // store call
        this.imcCode.store(fullCall, call);
    }

    @Override
    public void visit(Binary binary) {
        // get nodes
        binary.left.accept(this);
        binary.right.accept(this);
        IRNode left = this.imcCode.valueFor(binary.left).get();
        IRNode right = this.imcCode.valueFor(binary.right).get();

        // convert statements to expressions
//        if (left instanceof IRStmt)
//            left = new EseqExpr((IRStmt) left, new ConstantExpr(0));
//        if (right instanceof IRStmt)
//            right = new EseqExpr((IRStmt) right, new ConstantExpr(0));
//        if (left instanceof SeqStmt statements)
//            left = ((ExpStmt)statements.statements.get(0)).expr;
//        if (right instanceof SeqStmt statements)
//            right = ((ExpStmt)statements.statements.get(0)).expr;

        if (left instanceof IRStmt || right instanceof IRStmt)
            throw new  UnsupportedOperationException("What are you doing my guy? Error in IRCodeGenerator Binary!!!");

        // check operators and store
        if (binary.operator == Binary.Operator.ASSIGN) {
            if (binary.right instanceof Binary rightBinary) {
                if (rightBinary.operator == Binary.Operator.ARR)
                    right = new MemExpr((IRExpr) right);
            }
            MoveStmt move = new MoveStmt((IRExpr) left, (IRExpr) right);
            this.imcCode.store(move, binary);
        } else if (binary.operator == Binary.Operator.ARR) {
            Type.Array arrayType = (Type.Array) this.types.valueFor(binary.left).get();
            int elementSize = arrayType.type.sizeInBytes();
            BinopExpr offset = new BinopExpr((IRExpr) right, new ConstantExpr(elementSize), BinopExpr.Operator.MUL);
            BinopExpr pointer = new BinopExpr((IRExpr) left, offset, BinopExpr.Operator.ADD);
//            MemExpr value = new MemExpr(pointer);
            this.imcCode.store(pointer, binary);
        } else {
            if (binary.left instanceof Binary leftBinary) {
                if (leftBinary.operator == Binary.Operator.ARR)
                    left = new MemExpr((IRExpr) left);
            }
            if (binary.right instanceof Binary rightBinary) {
                if (rightBinary.operator == Binary.Operator.ARR)
                    right = new MemExpr((IRExpr) right);
            }
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
            if (tmp instanceof IRExpr) {
                if (expr instanceof Binary binary) {
                    if (binary.operator == Binary.Operator.ARR)
                        tmp = new MemExpr((IRExpr) tmp);
                }
                tmp = new ExpStmt((IRExpr) tmp);
            }
            statements.add((IRStmt) tmp);
        }
        if (statements.size() == 1) {
            if (statements.get(0) instanceof ExpStmt)
                tmp = ((ExpStmt) statements.get(0)).expr;
            else
                tmp = statements.get(0);
            this.imcCode.store(tmp, block);
            return;
        }
        if (statements.get(statements.size() - 1) instanceof ExpStmt last) {
            SeqStmt seq = new SeqStmt(statements.subList(0, statements.size() - 1));
            EseqExpr eseqExpr = new EseqExpr(seq, last.expr);
            this.imcCode.store(eseqExpr, block);
            return;
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
        IRNode counter = this.imcCode.valueFor(forLoop.counter).get();
//        if (counter instanceof IRStmt)
//            counter = new EseqExpr((IRStmt) counter, new ConstantExpr(0));
        IRNode low = this.imcCode.valueFor(forLoop.low).get();
        if (forLoop.low instanceof Binary binary) {
            if (binary.operator == Binary.Operator.ARR)
                low = new MemExpr((IRExpr) low);
        }
//        if (low instanceof IRStmt)
//            low = new EseqExpr((IRStmt) low, new ConstantExpr(0));
        IRNode high = this.imcCode.valueFor(forLoop.high).get();
        if (forLoop.high instanceof Binary binary) {
            if (binary.operator == Binary.Operator.ARR)
                high = new MemExpr((IRExpr) high);
        }
//        if (high instanceof IRStmt)
//            high = new EseqExpr((IRStmt) high, new ConstantExpr(0));
        IRNode step = this.imcCode.valueFor(forLoop.step).get();
        if (forLoop.step instanceof Binary binary) {
            if (binary.operator == Binary.Operator.ARR)
                step = new MemExpr((IRExpr) step);
        }
//        if (step instanceof IRStmt)
//            step = new EseqExpr((IRStmt) step, new ConstantExpr(0));

        if (
                counter instanceof IRStmt
                || low instanceof IRStmt
                || high instanceof IRStmt
                || step instanceof IRStmt
        )
            throw new  UnsupportedOperationException("What are you doing my guy? Error in IRCodeGenerator forLoop!!!");

        MoveStmt init = new MoveStmt((IRExpr) counter, (IRExpr) low);
        BinopExpr addition = new BinopExpr((IRExpr) counter, (IRExpr) step, BinopExpr.Operator.ADD);
        MoveStmt increment = new MoveStmt((IRExpr) counter, addition);
        BinopExpr condition = new BinopExpr((IRExpr) counter, (IRExpr) high, BinopExpr.Operator.LT);
        CJumpStmt cjump = new CJumpStmt(condition, loopBody, loopEnd);
        JumpStmt jump = new JumpStmt(loopStart);

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
        Def definition = this.definitions.valueFor(name).get();
        Access access = this.accesses.valueFor(definition).get();
        IRExpr value;
        if (access instanceof Access.Global) {
            Access.Global global = (Access.Global) access;
            NameExpr nameExpr = new NameExpr(global.label);
            value = nameExpr;
        } else {
            // needed variables
            ConstantExpr offset;
            int variableLevel;

            // get offset and variable level
            if (access instanceof Access.Local) {
                Access.Local local = (Access.Local) access;
                offset = new ConstantExpr(local.offset);
                variableLevel = local.staticLevel;
            } else {
                Access.Parameter parameter = (Access.Parameter) access;
                offset = new ConstantExpr(parameter.offset);
                variableLevel = parameter.staticLevel;
            }

            // get correct frame pointer
            int currentLevel = this.currentFrame.staticLevel;
            int levelDifference = currentLevel - variableLevel;
            IRExpr framePointer = NameExpr.FP();
            for (int i = 0; i < levelDifference; i++)
                framePointer = new MemExpr(framePointer);

            // get pointer
            BinopExpr pointer = new BinopExpr(framePointer, offset, BinopExpr.Operator.ADD);
            value = pointer;

            // if array type and parameter, dereference
            if (this.types.valueFor(name).get().isArray() && access instanceof Access.Parameter)
                value = new MemExpr(value);
        }

        // check if array
        if (!this.types.valueFor(name).get().isArray())
            value = new MemExpr(value);

        // store
        this.imcCode.store(value, name);
    }

    @Override
    public void visit(IfThenElse ifThenElse) {
        // labels
        Frame.Label trueLabel = Frame.Label.nextAnonymous();
        Frame.Label falseLabel = Frame.Label.nextAnonymous();
        Frame.Label endLabel = Frame.Label.nextAnonymous();
        LabelStmt trueLabelStmt = new LabelStmt(trueLabel);
        LabelStmt falseLabelStmt = new LabelStmt(falseLabel);
        LabelStmt endLabelStmt = new LabelStmt(endLabel);

        // condition
        ifThenElse.condition.accept(this);
        IRExpr condition = (IRExpr) this.imcCode.valueFor(ifThenElse.condition).get();
        if (ifThenElse.condition instanceof Binary binary) {
            if (binary.operator == Binary.Operator.ARR)
                condition = new MemExpr(condition);
        }
        CJumpStmt cjump = new CJumpStmt(condition, trueLabel, falseLabel);

        // assemble with body
        List<IRStmt> statements = new ArrayList<>();
        IRNode tmp;
        statements.add(cjump);
        statements.add(trueLabelStmt);
        ifThenElse.thenExpression.accept(this);
        tmp = this.imcCode.valueFor(ifThenElse.thenExpression).get();
        if (tmp instanceof IRExpr) {
            if (ifThenElse.thenExpression instanceof Binary binary) {
                if (binary.operator == Binary.Operator.ARR)
                    tmp = new MemExpr((IRExpr) tmp);
            }
            tmp = new ExpStmt((IRExpr) tmp);
        }
        statements.add((IRStmt) tmp);
        statements.add(new JumpStmt(endLabel));
        statements.add(falseLabelStmt);
        if (ifThenElse.elseExpression.isPresent()) {
            ifThenElse.elseExpression.get().accept(this);
            tmp = this.imcCode.valueFor(ifThenElse.elseExpression.get()).get();
            if (tmp instanceof IRExpr) {
                if (ifThenElse.elseExpression.get() instanceof Binary binary) {
                    if (binary.operator == Binary.Operator.ARR)
                        tmp = new MemExpr((IRExpr) tmp);
                }
                tmp = new ExpStmt((IRExpr) tmp);
            }
            statements.add((IRStmt) tmp);
        }
        statements.add(endLabelStmt);

        // create SeqStmt
        SeqStmt seq = new SeqStmt(statements);
        this.imcCode.store(seq, ifThenElse);
    }

    @Override
    public void visit(Literal literal) {
        Atom.Type literalType = literal.type;
        IRExpr value = switch (literalType) {
            case INT -> new ConstantExpr(Integer.parseInt(literal.value));
            case LOG -> new ConstantExpr(literal.value.equals("true") ? 1 : 0);
            case STR -> {
                Label label = Label.nextAnonymous();
                Access.Global globalAcces = new Access.Global(
                        literal.value.length() * Constants.WordSize,
                        label
                );
                Chunk.DataChunk string = new Chunk.DataChunk(globalAcces, literal.value);
                this.chunks.add(string);

                // getting pointer to string
                yield new NameExpr(label);
            }
        };
        this.imcCode.store(value, literal);
    }

    @Override
    public void visit(Unary unary) {
        unary.expr.accept(this);
        IRNode expr = this.imcCode.valueFor(unary.expr).get();
//        if (expr instanceof IRStmt)
//            expr = new EseqExpr((IRStmt) expr, new ConstantExpr(0));
        if (expr instanceof IRStmt)
            throw new  UnsupportedOperationException("What are you doing my guy? Error in IRCodeGenerator Unary!!!");

        if (unary.expr instanceof Binary binary) {
            if (binary.operator == Binary.Operator.ADD)
                expr = new MemExpr((IRExpr) expr);
        }

        ConstantExpr zero = new ConstantExpr(0);
        BinopExpr unop = switch (unary.operator) {
            case ADD -> new BinopExpr(zero, (IRExpr) expr, BinopExpr.Operator.ADD);
            case SUB -> new BinopExpr(zero, (IRExpr) expr, BinopExpr.Operator.SUB);
            case NOT -> new BinopExpr(new ConstantExpr(1), (IRExpr) expr, BinopExpr.Operator.SUB);
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
        if (whileLoop.condition instanceof Binary binary) {
            if (binary.operator == Binary.Operator.ARR)
                condition = new MemExpr(condition);
        }

        CJumpStmt cjump = new CJumpStmt(condition, loopBody, loopEnd);
        JumpStmt jump = new JumpStmt(loopStart);

        // assemble with body
        whileLoop.body.accept(this);
        IRNode body = this.imcCode.valueFor(whileLoop.body).get();
        if (body instanceof IRExpr) {
            if (whileLoop.body instanceof Binary binary) {
                if (binary.operator == Binary.Operator.ARR)
                    body = new MemExpr((IRExpr) body);
            }
            body = new ExpStmt((IRExpr) body);
        }
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

        if (funDef.body instanceof Binary binary) {
            if (binary.operator == Binary.Operator.ARR)
                body = new MemExpr((IRExpr) body);
        }

        // move(return value placement, koda funckije)
//        List<IRStmt> prevStatements = new ArrayList<>();
//        IRExpr returnValue;
//        if (body instanceof IRExpr)
//            returnValue = (IRExpr) body;
//        else {
//            if (body instanceof SeqStmt statements) {
//                IRStmt last = statements.statements.get(statements.statements.size() - 1);
//                if (last instanceof ExpStmt expStmt) {
//                    returnValue = expStmt.expr;
//                    prevStatements = statements.statements.subList(0, statements.statements.size() - 1);
//                } else
//                    throw new UnsupportedOperationException("Last statement should be ExpStmt");
//            } else
//                throw new UnsupportedOperationException("Should be statements in function!!!");
//        }

        MemExpr location = new MemExpr(NameExpr.FP());
        MoveStmt saveReturn = new MoveStmt(location, (IRExpr) body);
//        prevStatements.add(saveReturn);
//        SeqStmt statements = new SeqStmt(prevStatements);

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
