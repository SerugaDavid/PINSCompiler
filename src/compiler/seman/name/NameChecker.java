/**
 * @ Author: turk
 * @ Description: Preverjanje in razreševanje imen.
 */

package compiler.seman.name;

import static common.RequireNonNull.requireNonNull;

import common.Report;
import compiler.common.Visitor;
import compiler.lexer.Position;
import compiler.parser.ast.def.*;
import compiler.parser.ast.def.FunDef.Parameter;
import compiler.parser.ast.expr.*;
import compiler.parser.ast.type.*;
import compiler.seman.common.NodeDescription;
import compiler.seman.name.env.SymbolTable;
import compiler.seman.name.env.SymbolTable.DefinitionAlreadyExistsException;

import java.util.ArrayList;
import java.util.Optional;

public class NameChecker implements Visitor {
    /**
     * Opis vozlišč, ki jih povežemo z njihovimi
     * definicijami.
     */
    private NodeDescription<Def> definitions;

    /**
     * Simbolna tabela.
     */
    private SymbolTable symbolTable;

    /**
     * Ustvari nov razreševalnik imen.
     */
    public NameChecker(
        NodeDescription<Def> definitions,
        SymbolTable symbolTable
    ) {
        requireNonNull(definitions, symbolTable);
        this.definitions = definitions;
        this.symbolTable = symbolTable;
    }

    private boolean isSTDLIB(String name) {
        return name.equals("print_int") || name.equals("print_str") || name.equals("print_log") || name.equals("rand_int") || name.equals("seed");
    }

    @Override
    public void visit(Call call) {
        /**
         * Call expression
         * Watch out for variable or type calls instead of function calls
         */

        Optional<Def> opDef = this.symbolTable.definitionFor(call.name);
        if (opDef.isEmpty()) {
            if (this.isSTDLIB(call.name)) {
                for (Expr expr : call.arguments) {
                    expr.accept(this);
                }
                return;
            }
            Report.error(call.position, "Function " + call.name + " is not defined. Called at: " + call.position);
        }
        Def def = opDef.get();

        // Check if it is a function call
        if (def instanceof VarDef) {
            Report.error(call.position, "Variables are not callable. Called at: " + call.position);
        } else if (def instanceof TypeDef) {
            Report.error(call.position, "Types are not callable. Called at: " + call.position);
        } else {
            // Check parameter names
            for (Expr expr : call.arguments) {
                expr.accept(this);
            }
        }

        this.definitions.store(def, call);
    }

    @Override
    public void visit(Binary binary) {
        /**
         * Binary expression
         * Left and right expression will handle themselves in other visit methods
         */

        binary.left.accept(this);
        binary.right.accept(this);
    }

    @Override
    public void visit(Block block) {
        /**
         * Block expression
         * Visit all expressions in the block
         */

        for (Expr expr : block.expressions) {
            expr.accept(this);
        }
    }

    @Override
    public void visit(For forLoop) {
        /**
         * For loop expression
         * How do we handle counter? Probably is just defined in the where clause
         */

        forLoop.counter.accept(this);
        forLoop.low.accept(this);
        forLoop.high.accept(this);
        forLoop.step.accept(this);
        forLoop.body.accept(this);
    }

    @Override
    public void visit(Name name) {
        /**
         * Name expression
         * Check if name is defined
         */

        Optional<Def> opDef = this.symbolTable.definitionFor(name.name);
        if (opDef.isEmpty()) {
            Report.error(name.position, "Name " + name + " is not defined. Called at: " + name.position);
        }

        Def def = opDef.get();
        if (def instanceof TypeDef) {
            Report.error(name.position, "Name " + name + " is a type. Called at: " + name.position);
        } else if (def instanceof FunDef) {
            Report.error(name.position, "Name " + name + " is a function. Called at: " + name.position);
        }
        this.definitions.store(def, name);
    }

    @Override
    public void visit(IfThenElse ifThenElse) {
        /**
         * If then else expression
         * Watch out for else expression because it may be optional
         */

        ifThenElse.condition.accept(this);
        ifThenElse.thenExpression.accept(this);
        Optional<Expr> elseExpr = ifThenElse.elseExpression;
        if (elseExpr.isPresent()) {
            elseExpr.get().accept(this);
        }
    }

    @Override
    public void visit(Literal literal) {
        /**
         * Literal expression
         * Literals are fine, no need for name checking
         */
    }

    @Override
    public void visit(Unary unary) {
        /**
         * Unary expression
         * Visit only one expression
         */

        unary.expr.accept(this);
    }

    @Override
    public void visit(While whileLoop) {
        /**
         * While loop expression
         * Check condition and body
         */

        whileLoop.condition.accept(this);
        whileLoop.body.accept(this);
    }

    @Override
    public void visit(Where where) {
        /**
         * Where expression, Fun stuff (I guess not so fun, easy)
         * We push the scope on our symbol table
         * First we have to make all declarations in the where clause (handled by the defs visitor)
         *  We declare all different definitions
         *  Check for named types
         * Visit the expression inside the where clause
         * Pop the scope
         */

        this.symbolTable.pushScope();

        where.defs.accept(this);
        where.expr.accept(this);

        this.symbolTable.popScope();
    }

    @Override
    public void visit(Defs defs) {
        /**
         * Defs visitor
         * First we add all the definitions to the symbol table
         * Then we check all the definitions for type names
         */

        // Add definitions to symbol table.
        for (Def def : defs.definitions) {
            try {
                this.symbolTable.insert(def);
            } catch (DefinitionAlreadyExistsException e) {
                Report.error(def.position, "Definition '" + def.name + "' already defined at: " + def.position);
            }
        }

        // Check definitions.
        for (Def def : defs.definitions) {
            def.accept(this);
        }
    }

    @Override
    public void visit(FunDef funDef) {
        /**
         * Function definition
         * First we check if the function type is defined
         * We check parameter types
         * Push scope
         * Define parameters
         * Visit body
         * Pop scope
         */

        // check type definition
        funDef.type.accept(this);

        // check parameter type definitions
        for (Parameter parameter: funDef.parameters) {
            parameter.type.accept(this);
        }

        // tle se ga rekurzivno skustiš in pohendlaš te nove scope
        this.symbolTable.pushScope();

        for (Parameter parameter : funDef.parameters) {
            try {
                this.symbolTable.insert(parameter);
            } catch (DefinitionAlreadyExistsException e) {
                Report.error(parameter.position, "Parameter '" + parameter.name + "' already defined at: " + parameter.position + " in function '" + funDef.name + "'");
            }
        }
        funDef.body.accept(this);

        this.symbolTable.popScope();
    }

    @Override
    public void visit(TypeDef typeDef) {
        /**
         * Type definition
         * Check if type of type is defined
         */

        // check type definition
        typeDef.type.accept(this);
    }

    @Override
    public void visit(VarDef varDef) {
        /**
         * Variable definition
         * Check if type of variable is defined
         */

        // check type definition
        varDef.type.accept(this);
    }

    @Override
    public void visit(Parameter parameter) {
        /**
         * Parameter definition
         * IDK if this is needed
         * Checks made in FunDef
         */
    }

    @Override
    public void visit(Array array) {
        /**
         * Array type
         * Visit the type of the array
         */

        array.type.accept(this);
    }

    @Override
    public void visit(Atom atom) {
        /**
         * Atom type
         * IDK if this is needed
         * Checks made in TypeName visitor
         */
    }

    @Override
    public void visit(TypeName name) {
        /**
         * Type name
         * Get definition from symbol table
         * Check if it exists
         * Check if it is a type
         */

        Optional<Def> opDef = this.symbolTable.definitionFor(name.identifier);
        if (opDef.isEmpty()) {
            Report.error(name.position, "Type '" + name.identifier + "' at: " + name.position + "not defined");
        }
        Def def = opDef.get();
        if (!(def instanceof TypeDef)) {
            Report.error(name.position, "Type '" + name.identifier + "' at: " + name.position + " is not a valid type");
        }
        this.definitions.store(def, name);
    }
}
