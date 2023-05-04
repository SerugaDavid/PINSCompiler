/**
 * @ Author: turk
 * @ Description: Binarni izraz.
 */

package compiler.ir.code.expr;

import compiler.parser.ast.expr.Binary;

import static common.RequireNonNull.requireNonNull;

public class BinopExpr extends IRExpr {
    /**
     * Levi operand.
     */
    public final IRExpr lhs;

    /**
     * Desni operand.
     */
    public final IRExpr rhs;

    /**
     * Operator.
     */
    public final Operator op;

    public BinopExpr(IRExpr lhs, IRExpr rhs, Operator op) {
        requireNonNull(lhs, rhs, op);
        this.lhs = lhs;
        this.rhs = rhs;
        this.op = op;
    }

    public static enum Operator {
        ADD, SUB, MUL, DIV, MOD, // aritmetični
        AND, OR, // logični
        EQ, NEQ, LT, GT, LEQ, GEQ // primerjalni
    }

    public static Operator convertOp(Binary.Operator operator) {
        return switch (operator) {
            case ADD -> Operator.ADD;
            case SUB -> Operator.SUB;
            case MUL -> Operator.MUL;
            case DIV -> Operator.DIV;
            case MOD -> Operator.MOD;
            case AND -> Operator.AND;
            case OR -> Operator.OR;
            case EQ -> Operator.EQ;
            case NEQ -> Operator.NEQ;
            case LT -> Operator.LT;
            case GT -> Operator.GT;
            case LEQ -> Operator.LEQ;
            case GEQ -> Operator.GEQ;
            default -> throw new IllegalArgumentException("Unknown operator: " + operator);
        };
    }
}
