#ifndef SEMANTIC_H_INCLUDED
#define SEMANTIC_H_INCLUDED

typedef struct ScanHelper ScanHelper;

struct ScanHelper {
};

static void scan_expression(ScanHelper* helper,  Expression* expr, Scope* scope);
static void scan_assignment(ScanHelper* helper,  Statement* statement, Scope* scope);
static void scan_controlflow(ScanHelper* helper, ControlFlow* controlflow, Scope* scope);
static void scan_statement(ScanHelper* helper,   Statement* statement, Code* code);
static void scan_code(ScanHelper* helper,        Code* code);
static void scan_function(ScanHelper* helper,    Function* func);
static void scan_module(Module* module);

#endif // SEMANTIC_H_INCLUDED

