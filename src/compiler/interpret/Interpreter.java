/**
 * @ Author: turk
 * @ Description: Navidezni stroj (intepreter).
 */

package compiler.interpret;

import static common.RequireNonNull.requireNonNull;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.nio.charset.Charset;
import java.util.Optional;
import java.util.Random;

import common.Constants;
import compiler.frm.Frame;
import compiler.gen.Memory;
import compiler.ir.chunk.Chunk.CodeChunk;
import compiler.ir.code.IRNode;
import compiler.ir.code.expr.*;
import compiler.ir.code.stmt.*;
import compiler.ir.IRPrettyPrint;

public class Interpreter {
    /**
     * Pomnilnik navideznega stroja.
     */
    private Memory memory;
    
    /**
     * Izhodni tok, kamor izpisujemo rezultate izvajanja programa.
     * 
     * V primeru, da rezultatov ne želimo izpisovati, nastavimo na `Optional.empty()`.
     */
    private Optional<PrintStream> outputStream;

    /**
     * Generator naključnih števil.
     */
    private Random random;

    /**
     * Skladovni kazalec (kaže na dno sklada).
     */
    private int stackPointer;

    /**
     * Klicni kazalec (kaže na vrh aktivnega klicnega zapisa).
     */
    private int framePointer;

    public Interpreter(Memory memory, Optional<PrintStream> outputStream) {
        requireNonNull(memory, outputStream);
        this.memory = memory;
        this.outputStream = outputStream;
        this.stackPointer = memory.size - Constants.WordSize;
        this.framePointer = memory.size - Constants.WordSize;
    }

    // --------- izvajanje navideznega stroja ----------

    public void interpret(CodeChunk chunk) {
        memory.stM(framePointer + Constants.WordSize, 0); // argument v funkcijo main
        memory.stM(framePointer - chunk.frame.oldFPOffset(), framePointer); // oldFP
        internalInterpret(chunk);
    }

    private Object internalInterpret(CodeChunk chunk) {
        // @TODO: Nastavi FP in SP na nove vrednosti!
        this.memory.stM(this.framePointer - chunk.frame.oldFPOffset(), this.framePointer);
        this.framePointer = this.stackPointer;
        this.stackPointer -= chunk.frame.size();
        this.memory.registerLabel(Frame.Label.named(Constants.framePointer), this.framePointer);
        this.memory.registerLabel(Frame.Label.named(Constants.stackPointer), this.stackPointer);
        
        Object result = null;
        if (chunk.code instanceof SeqStmt seq) {
            for (int pc = 0; pc < seq.statements.size(); pc++) {
                var stmt = seq.statements.get(pc);
                result = execute(stmt, false);
                if (result instanceof Frame.Label label) {
                    for (int q = 0; q < seq.statements.size(); q++) {
                        if (seq.statements.get(q) instanceof LabelStmt labelStmt && labelStmt.label.equals(label)) {
                            pc = q;
                            break;
                        }
                    }
                }
            }
        } else {
            throw new RuntimeException("Linearize code!");
        }

        // @TODO: Ponastavi FP in SP na stare vrednosti!
        this.stackPointer = this.framePointer;
        this.framePointer = (int) memory.ldM(framePointer - chunk.frame.oldFPOffset());
        this.memory.registerLabel(Frame.Label.named(Constants.framePointer), this.framePointer);
        this.memory.registerLabel(Frame.Label.named(Constants.stackPointer), this.stackPointer);

        return result;
    }

    private Object execute(IRStmt stmt, boolean isAssign) {
        if (stmt instanceof CJumpStmt cjump) {
            return execute(cjump, isAssign);
        } else if (stmt instanceof ExpStmt exp) {
            return execute(exp, isAssign);
        } else if (stmt instanceof JumpStmt jump) {
            return execute(jump, isAssign);
        } else if (stmt instanceof LabelStmt label) {
            return null;
        } else if (stmt instanceof MoveStmt move) {
            return execute(move, isAssign);
        } else {
            throw new RuntimeException("Cannot execute this statement!");
        }
    }

    private Object execute(CJumpStmt cjump, boolean isAssign) {
        boolean condition = (boolean) execute(cjump.condition, isAssign);
        if (condition)
            return cjump.thenLabel;
        return cjump.elseLabel;
    }

    private Object execute(ExpStmt exp, boolean isAssign) {
        return execute(exp.expr, isAssign);
    }

    private Object execute(JumpStmt jump, boolean isAssign) {
        return jump.label;
    }

    private Object execute(MoveStmt move, boolean isAssign) {
        Object src = execute(move.src, false);

        if (move.dst instanceof MemExpr memExpr) {
            Object dst = execute(memExpr, true);
            memory.stM(toInt(dst), src);
        } else if (move.dst instanceof TempExpr tempExpr) {
            Frame.Temp dst = (Frame.Temp) execute(tempExpr, true);
            memory.stT(dst, src);
        } else {
            throw new RuntimeException("Cannot execute MOVE to this destination!");
        }

        return src;
    }

    private Object execute(IRExpr expr, boolean isAssign) {
        if (expr instanceof BinopExpr binopExpr) {
            return execute(binopExpr, isAssign);
        } else if (expr instanceof CallExpr callExpr) {
            return execute(callExpr, isAssign);
        } else if (expr instanceof ConstantExpr constantExpr) {
            return execute(constantExpr, isAssign);
        } else if (expr instanceof EseqExpr eseqExpr) {
            throw new RuntimeException("Cannot execute ESEQ; linearize code!");
        } else if (expr instanceof MemExpr memExpr) {
            return execute(memExpr, isAssign);
        } else if (expr instanceof NameExpr nameExpr) {
            return execute(nameExpr, isAssign);
        } else if (expr instanceof TempExpr tempExpr) {
            return execute(tempExpr, isAssign);
        } else {
            throw new IllegalArgumentException("Unknown expr type");
        }
    }

    private Object execute(BinopExpr binop, boolean isAssign) {
        Object left = execute(binop.lhs, isAssign);

        Object right = execute(binop.rhs, false/* Maybe false */);

        return switch (binop.op) {
            // logical operators
            case GT -> toInt(left) > toInt(right);
            case LT -> toInt(left) < toInt(right);
            case GEQ -> toInt(left) >= toInt(right);
            case LEQ -> toInt(left) <= toInt(right);
            case EQ -> toInt(left) == toInt(right);
            case NEQ -> toInt(left) != toInt(right);
            case AND -> toBool(left) && toBool(right);
            case OR -> toBool(left) || toBool(right);

            // arithmetic operators
            case ADD -> toInt(left) + toInt(right);
            case SUB -> toInt(left) - toInt(right);
            case MUL -> toInt(left) * toInt(right);
            case DIV -> toInt(left) / toInt(right);
            case MOD -> toInt(left) % toInt(right);
        };
    }

    private Object execute(CallExpr call, boolean isAssign) {
        if (call.label.name.equals(Constants.printIntLabel)) {
            if (call.args.size() != 2) { throw new RuntimeException("Invalid argument count!"); }
            var arg = execute(call.args.get(1), false);
            outputStream.ifPresent(stream -> stream.println(arg));
            return 0;
        } else if (call.label.name.equals(Constants.printStringLabel)) {
            if (call.args.size() != 2) { throw new RuntimeException("Invalid argument count!"); }
            var address = execute(call.args.get(1), false);
            var res = memory.ldM(toInt(address));
            outputStream.ifPresent(stream -> stream.println("\""+res+"\""));
            return 0;
        } else if (call.label.name.equals(Constants.printLogLabel)) {
            if (call.args.size() != 2) { throw new RuntimeException("Invalid argument count!"); }
            var arg = execute(call.args.get(1), false);
            outputStream.ifPresent(stream -> stream.println(toBool(arg)));
            return 0;
        } else if (call.label.name.equals(Constants.randIntLabel)) {
            if (call.args.size() != 3) { throw new RuntimeException("Invalid argument count!"); }
            var min = toInt(execute(call.args.get(1), false));
            var max = toInt(execute(call.args.get(2), false));
            return random.nextInt(min, max);
        } else if (call.label.name.equals(Constants.seedLabel)) {
            if (call.args.size() != 2) { throw new RuntimeException("Invalid argument count!"); }
            var seed = toInt(execute(call.args.get(1), false));
            random = new Random(seed);
            return 0;
        } else if (memory.ldM(call.label) instanceof CodeChunk chunk) {
            // TODO: tle moraš pogruntat, nek return value
            // TODO: kak ga dobit in kako ga vrnit
            // TODO: ?????????????????????????????????????
            return internalInterpret(chunk);
        } else {
            throw new RuntimeException("Only functions can be called!");
        }
    }

    private Object execute(ConstantExpr constant, boolean isAssign) {
        return constant.constant;
    }

    private Object execute(MemExpr mem, boolean isAssign) {
        Object pointer = execute(mem.expr, isAssign);
        Object value = null;
        if (pointer instanceof Frame.Label label)
            value = isAssign ? label : this.memory.ldM(label);
        else if (pointer instanceof Integer address)
            value = isAssign ? address : this.memory.ldM(address);
        return value;
    }

    // TODO: Old MemExpr execute
//    private AddressValuePair execute(MemExpr mem) {
//        Object pointer = execute(mem.expr);
//        if (pointer instanceof Frame.Label label) {
//            Object value = this.memory.ldM(label);
//            return new AddressValuePair(-1, value);
//        }
//        if (pointer instanceof Integer address) {
//            Object value;
//            try {
//                value = this.memory.ldM(address);
//            } catch (IllegalArgumentException e) {
//                value = null;
//            }
//            return new AddressValuePair(address, value);
//        }
//        throw new RuntimeException("Cannot execute MEM with this address!");
//    }

    private Object execute(NameExpr name, boolean isAssign) {
        Frame.Label value = name.label;
        if (value.name.equals(Constants.framePointer))
            return this.framePointer;
        if (value.name.equals(Constants.stackPointer))
            return this.stackPointer;
        return value;
    }

    private Object execute(TempExpr temp, boolean isAssign) {
        if (isAssign)
            return temp.temp;
        return this.memory.ldT(temp.temp);
    }

    // ----------- pomožne funkcije -----------

    private int toInt(Object obj) {
        if (obj instanceof Integer integer) {
            return integer;
        }
        throw new IllegalArgumentException("Could not convert obj to integer!");
    }

    private boolean toBool(Object obj) {
        return toInt(obj) == 0 ? false : true;
    }

    private int toInt(boolean bool) {
        return bool ? 1 : 0;
    }

    private String prettyDescription(IRNode ir, int indent) {
        var os = new ByteArrayOutputStream();
        var ps = new PrintStream(os);
        new IRPrettyPrint(ps, indent).print(ir);
        return os.toString(Charset.defaultCharset());
    }

    private String prettyDescription(IRNode ir) {
        return prettyDescription(ir, 2);
    }

    private void prettyPrint(IRNode ir, int indent) {
        System.out.println(prettyDescription(ir, indent));
    }

    private void prettyPrint(IRNode ir) {
        System.out.println(prettyDescription(ir));
    }

    /**
     * Pairs an address with a value.
     */
    private class AddressValuePair {
        public final int address;
        public final Object value;

        public AddressValuePair(int address, Object value) {
            this.address = address;
            this.value = value;
        }
    }
}
