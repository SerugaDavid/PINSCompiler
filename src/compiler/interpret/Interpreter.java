/**
 * @ Author: turk
 * @ Description: Navidezni stroj (intepreter).
 */

package compiler.interpret;

import static common.RequireNonNull.requireNonNull;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.nio.charset.Charset;
import java.util.HashMap;
import java.util.Map;
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
import compiler.parser.ast.expr.Binary;

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
        internalInterpret(chunk, new HashMap<>());
    }

    private void internalInterpret(CodeChunk chunk, Map<Frame.Temp, Object> temps) {
        // @TODO: Nastavi FP in SP na nove vrednosti!
        this.memory.stM(this.stackPointer - chunk.frame.oldFPOffset(), this.framePointer);
        this.framePointer = this.stackPointer;
        this.stackPointer -= chunk.frame.size();
        this.memory.registerLabel(Frame.Label.named(Constants.framePointer), this.framePointer);
        this.memory.registerLabel(Frame.Label.named(Constants.stackPointer), this.stackPointer);
        
        Object result = null;
        if (chunk.code instanceof SeqStmt seq) {
            for (int pc = 0; pc < seq.statements.size(); pc++) {
                var stmt = seq.statements.get(pc);
                result = execute(stmt, temps);
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
    }

    private Object execute(IRStmt stmt, Map<Frame.Temp, Object> temps) {
        if (stmt instanceof CJumpStmt cjump) {
            return execute(cjump, temps);
        } else if (stmt instanceof ExpStmt exp) {
            return execute(exp, temps);
        } else if (stmt instanceof JumpStmt jump) {
            return execute(jump, temps);
        } else if (stmt instanceof LabelStmt label) {
            return null;
        } else if (stmt instanceof MoveStmt move) {
            return execute(move, temps);
        } else {
            throw new RuntimeException("Cannot execute this statement!");
        }
    }

    private Object execute(CJumpStmt cjump, Map<Frame.Temp, Object> temps) {
        int condition = (int) execute(cjump.condition, temps);
        if (condition == 1)
            return cjump.thenLabel;
        return cjump.elseLabel;
    }

    private Object execute(ExpStmt exp, Map<Frame.Temp, Object> temps) {
        return execute(exp.expr, temps);
    }

    private Object execute(JumpStmt jump, Map<Frame.Temp, Object> temps) {
        return jump.label;
    }

    private Object execute(MoveStmt move, Map<Frame.Temp, Object> temps) {
        Object src = execute(move.src, temps);

        if (move.dst instanceof MemExpr memExpr) {
            Object dst = execute(memExpr.expr, temps);
            memory.stM(toInt(dst), src);
        } else if (move.dst instanceof TempExpr tempExpr) {
            Frame.Temp dst = tempExpr.temp;
            temps.put(dst, src);
            memory.stT(dst, src);
        } else if (move.dst instanceof BinopExpr binopExpr) {
            Object dst = execute(binopExpr, temps);
            memory.stM(toInt(dst), src);
        } else {
            throw new RuntimeException("Cannot execute MOVE to this destination!");
        }

        return null;
    }

    private Object execute(IRExpr expr, Map<Frame.Temp, Object> temps) {
        if (expr instanceof BinopExpr binopExpr) {
            return execute(binopExpr, temps);
        } else if (expr instanceof CallExpr callExpr) {
            return execute(callExpr, temps);
        } else if (expr instanceof ConstantExpr constantExpr) {
            return execute(constantExpr, temps);
        } else if (expr instanceof EseqExpr eseqExpr) {
            throw new RuntimeException("Cannot execute ESEQ; linearize code!");
        } else if (expr instanceof MemExpr memExpr) {
            return execute(memExpr, temps);
        } else if (expr instanceof NameExpr nameExpr) {
            return execute(nameExpr, temps);
        } else if (expr instanceof TempExpr tempExpr) {
            return execute(tempExpr, temps);
        } else {
            throw new IllegalArgumentException("Unknown expr type");
        }
    }

    private Object execute(BinopExpr binop, Map<Frame.Temp, Object> temps) {
        Object left = execute(binop.lhs, temps);
        if (left instanceof Frame.Label label)
            left = this.memory.address(label);

        Object right = execute(binop.rhs, temps);
        if (right instanceof Frame.Label label)
            right = this.memory.address(label);

        Object out = switch (binop.op) {
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

        // check for boolean
        if (out instanceof Boolean)
            out = toInt((boolean) out);

        return out;
    }

    private Object execute(CallExpr call, Map<Frame.Temp, Object> temps) {
        if (call.label.name.equals(Constants.printIntLabel)) {
            if (call.args.size() != 2) { throw new RuntimeException("Invalid argument count!"); }
            var arg = execute(call.args.get(1), temps);
            outputStream.ifPresent(stream -> stream.println(arg));
            return 0;
        } else if (call.label.name.equals(Constants.printStringLabel)) {
            if (call.args.size() != 2) { throw new RuntimeException("Invalid argument count!"); }
            var address = execute(call.args.get(1), temps);
            var res = memory.ldM((Frame.Label)address);
            outputStream.ifPresent(stream -> stream.println("\""+res+"\""));
            return 0;
        } else if (call.label.name.equals(Constants.printLogLabel)) {
            if (call.args.size() != 2) { throw new RuntimeException("Invalid argument count!"); }
            var arg = execute(call.args.get(1), temps);
            outputStream.ifPresent(stream -> stream.println(toBool(arg)));
            return 0;
        } else if (call.label.name.equals(Constants.randIntLabel)) {
            if (call.args.size() != 3) { throw new RuntimeException("Invalid argument count!"); }
            var min = toInt(execute(call.args.get(1), temps));
            var max = toInt(execute(call.args.get(2), temps));
            return random.nextInt(min, max);
        } else if (call.label.name.equals(Constants.seedLabel)) {
            if (call.args.size() != 2) { throw new RuntimeException("Invalid argument count!"); }
            var seed = toInt(execute(call.args.get(1), temps));
            random = new Random(seed);
            return 0;
        } else if (memory.ldM(call.label) instanceof CodeChunk chunk) {
            for (int i = 0; i < call.args.size(); i++) {
                var arg = execute(call.args.get(i), temps);
                int address = this.stackPointer + i*Constants.WordSize;
                this.memory.stM(address, arg);
            }

            internalInterpret(chunk, temps);

            return this.memory.ldM(this.stackPointer);
        } else {
            throw new RuntimeException("Only functions can be called!");
        }
    }

    private Object execute(ConstantExpr constant, Map<Frame.Temp, Object> temps) {
        return constant.constant;
    }

    private Object execute(MemExpr mem, Map<Frame.Temp, Object> temps) {
        Object pointer = execute(mem.expr, temps);
        Object value = null;
        if (pointer instanceof Frame.Label label)
            value = this.memory.ldM(label);
        else if (pointer instanceof Integer address)
            value = this.memory.ldM(address);
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

    private Object execute(NameExpr name, Map<Frame.Temp, Object> temps) {
        Frame.Label value = name.label;
        if (value.name.equals(Constants.framePointer))
            return this.framePointer;
        if (value.name.equals(Constants.stackPointer))
            return this.stackPointer;
        return value;
    }

    private Object execute(TempExpr temp, Map<Frame.Temp, Object> temps) {
        return temps.get(temp.temp);
//        return this.memory.ldT(temp.temp);
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
