#ifndef SEMANTIC_H_INCLUDED
#define SEMANTIC_H_INCLUDED

void scan_expression(Module* module, Expression* expr, Scope* scope);
void scan_statement(Module* module, Statement* statement, Code* code);
void scan_code(Module* module, Code* code);
void scan_scope(Module* module, Scope* scope);
void scan_assignment(Module* module, Statement* statement, Scope* scope);
void scan_function(Module* module, Function* func);
void scan_controlflow(Module* module, ControlFlow* controlflow, Scope* scope);
void scan_module(Module* module);

#endif // SEMANTIC_H_INCLUDED

