# platypus_Interpreter
A front-end compiler implementation for a PLATYPUS specification.

This interpreter is designed to have centralized execution and
evaluations.

The program uses two main states of procedure. The err_state switches on
if a syntax error is found. Once this flag is switched on it cannot be
switched off again throughout the remainder of the parser. This flag 
prevents any expression evaluations or statement executions once 
switched on. 

Throughout the program you will find an execute flag being turned on and
off. This can be seen in statements that have conditional evaluations of
expressions. This stops say an ELSE clause from evaluating within a
selection statement if the preceding condition evaluates to true. 

A third flag specifically for the assignment statement tells an evaluation
of the interior assignment expression if there is a chance for an update 
of the left value's type. The type of a variable can only be changed
within an assignment statement.

Every call to match(), will analyze the lookahead token and add it to an
operator stack or the expression list if the err_state flag has not been
set.

Once an expression has successfully been parsed it will then evaluate
itself by calling the evaluate_expression() sending the global expression
list, built with each call to match, as an argument. This function handles
all expression evaluations.

Executable statements are the input and output statements. These two
statements will call the execute_statement() which handles the I/O
operations.
