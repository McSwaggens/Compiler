#ifndef SEMANTIC_H_INCLUDED
#define SEMANTIC_H_INCLUDED

void scan_expression(Expression* expr, Scope* scope);
void scan_statement(Statement* statement, Code* code);
void scan_code(Code* code);
void scan_scope(Scope* scope);
void scan_assignment(Statement* statement, Scope* scope);
void scan_function(Function* func);
void scan_controlflow(ControlFlow* controlflow, Scope* scope);
void scan_module(Module* module);

#endif // SEMANTIC_H_INCLUDED

