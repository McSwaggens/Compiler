#ifndef SEMANTIC_H_INCLUDED
#define SEMANTIC_H_INCLUDED

static void scan_expression(Module* module, Expression* expr, Scope* scope);
static void scan_statement(Module* module, Statement* statement, Code* code);
static void scan_code(Module* module, Code* code);
static void scan_scope(Module* module, Scope* scope);
static void scan_assignment(Module* module, Statement* statement, Scope* scope);
static void scan_function(Module* module, Function* func);
static void scan_controlflow(Module* module, ControlFlow* controlflow, Scope* scope);
static void scan_module(Module* module);

#endif // SEMANTIC_H_INCLUDED

