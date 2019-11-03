%language "c++"
%require "3.0.4"
%defines
%define parser_class_name{ mlaskal_parser }
%define api.token.constructor
%define api.token.prefix{DUTOK_}
%define api.value.type variant
%define parse.assert
%define parse.error verbose

%locations
%define api.location.type{ unsigned }

%code requires
{
	// this code is emitted to du3456g.hpp

	// allow references to semantic types in %type
#include "dutables.hpp"

	// avoid no-case warnings when compiling du3g.hpp
#pragma warning (disable:4065)

// adjust YYLLOC_DEFAULT macro for our api.location.type
#define YYLLOC_DEFAULT(res,rhs,N)	(res = (N)?YYRHSLOC(rhs, 1):YYRHSLOC(rhs, 0))
// supply missing YY_NULL in bfexpg.hpp
#define YY_NULL	0
#define YY_NULLPTR	0
}

%param{ mlc::yyscan_t2 yyscanner }	// formal name "yyscanner" is enforced by flex
%param{ mlc::MlaskalCtx* ctx }

%start mlaskal

%code
{
	// this code is emitted to du3456g.cpp

	// declare yylex here 
	#include "bisonflex.hpp"
	YY_DECL;

	// allow access to context 
	#include "dutables.hpp"

	// other user-required contents
	#include <assert.h>
	#include <stdlib.h>

    /* local stuff */
    using namespace mlc;

}

%token EOF	0	"end of file"

%token PROGRAM			/* program */
%token LABEL			    /* label */
%token CONST			    /* const */
%token TYPE			    /* type */
%token VAR			    /* var */
%token BEGIN			    /* begin */
%token END			    /* end */
%token PROCEDURE			/* procedure */
%token FUNCTION			/* function */
%token ARRAY			    /* array */
%token OF				    /* of */
%token GOTO			    /* goto */
%token IF				    /* if */
%token THEN			    /* then */
%token ELSE			    /* else */
%token WHILE			    /* while */
%token DO				    /* do */
%token REPEAT			    /* repeat */
%token UNTIL			    /* until */
%token FOR			    /* for */
%token OR				    /* or */
%token NOT			    /* not */
%token RECORD			    /* record */

/* literals */
%token<mlc::ls_id_index> IDENTIFIER			/* identifier */
%token<mlc::ls_int_index> UINT			    /* unsigned integer */
%token<mlc::ls_real_index> REAL			    /* real number */
%token<mlc::ls_str_index> STRING			    /* string */

/* delimiters */
%token SEMICOLON			/* ; */
%token DOT			    /* . */
%token COMMA			    /* , */
%token EQ				    /* = */
%token COLON			    /* : */
%token LPAR			    /* ( */
%token RPAR			    /* ) */
%token DOTDOT			    /* .. */
%token LSBRA			    /* [ */
%token RSBRA			    /* ] */
%token ASSIGN			    /* := */

/* grouped operators and keywords */
%token<mlc::DUTOKGE_OPER_REL> OPER_REL			    /* <, <=, <>, >=, > */
%token<mlc::DUTOKGE_OPER_SIGNADD> OPER_SIGNADD		    /* +, - */
%token<mlc::DUTOKGE_OPER_MUL> OPER_MUL			    /* *, /, div, mod, and */
%token<mlc::DUTOKGE_FOR_DIRECTION> FOR_DIRECTION		    /* to, downto */


/* dangling else */
%precedence THEN
%precedence ELSE

%%

mlaskal: PROGRAM 
         IDENTIFIER 
		 SEMICOLON 
		 block_p
		 DOT
;

block_common: block_p1_opt /* label block */
		 	  block_p2_opt /* const block */
		 	  block_p3_opt /* type block */
		 	  block_p4_opt /* var block */
;
body: BEGIN
      statement_list_opt
	  END
;

block_p: block_common
		 block_p5_opt /* procedures and functions */
		 body
;

block: block_common
	   body
;

block_p1_opt: %empty 
		    | LABEL uint_list_1 SEMICOLON
;

block_p2_opt: %empty
			| CONST block_p2_list
;
block_p2_list: IDENTIFIER EQ constant SEMICOLON block_p2_next
;
block_p2_next: %empty
			 | IDENTIFIER EQ constant SEMICOLON block_p2_next
;

block_p3_opt: %empty
		    | TYPE block_p3_list
;
block_p3_list: IDENTIFIER EQ type SEMICOLON block_p3_next
;
block_p3_next: %empty
			 | IDENTIFIER EQ type SEMICOLON block_p3_next
;

block_p4_opt: %empty
			| VAR block_p4_list
;
block_p4_list: identifier_list_1 COLON type SEMICOLON block_p4_next
;
block_p4_next: %empty
			 | identifier_list_1 COLON type SEMICOLON block_p4_next
;

block_p5_opt: %empty
			| block_p5_list
;
block_p5_list: procedure_header block SEMICOLON block_p5_next
			 | function_header block SEMICOLON block_p5_next
;
block_p5_next: %empty
			 | procedure_header block SEMICOLON block_p5_next
			 | function_header block SEMICOLON block_p5_next
;

uint_list_1: UINT
			 uint_next
;
uint_next: %empty
		 | COMMA UINT uint_next
;

constant: unsigned_constant
	    | OPER_SIGNADD UINT
		| OPER_SIGNADD REAL
;

 				//constant_identifier
unsigned_constant: UINT
				 | REAL
				 | STRING
;

pm_opt: %empty
	  | OPER_SIGNADD
;

//expressions
boolean_expression: expression
;
ordinal_expression: expression
;

expression_cmp: EQ
			  | OPER_REL
;

expression: simple_expression expression_next
;

expression_next: %empty
    		   | expression_cmp simple_expression
;

simple_expression: pm_opt
				   term
				   simple_expression_next
;
simple_expression_bin_op: OPER_SIGNADD
				        | OR
;
simple_expression_next: %empty
					  | simple_expression_bin_op
					    term
						simple_expression_next
;

term: factor
	  term_next
;

term_next: %empty
		 | OPER_MUL
		   factor
		   term_next
;

factor: unsigned_constant
	  | variable

   //function_identifier
	  | IDENTIFIER factor_parameters_opt
	  | LPAR expression RPAR
	  | NOT factor
;

factor_parameters_opt: %empty
					 | LPAR real_parameters RPAR
;

real_parameters: expression real_parameters_next
;

real_parameters_next: %empty
			        | COMMA expression real_parameters_next
;

variable: variable DOT IDENTIFIER
		| IDENTIFIER DOT IDENTIFIER
;


statement: UINT COLON stmt_1
		 | stmt_1

		   //procedure_identifier
	     | IDENTIFIER factor_parameters_opt
	  	 | GOTO UINT 
		 | BEGIN statement_list_opt END
		 | IF boolean_expression THEN statement ELSE statement
		 | IF boolean_expression THEN statement 
		 | WHILE boolean_expression DO statement
		 | REPEAT statement_list_opt UNTIL boolean_expression

		  //ordinal_type_variable_identifier
		 | FOR IDENTIFIER ASSIGN ordinal_expression FOR_DIRECTION ordinal_expression DO statement
;

statement_list_opt: %empty
 			      | statements
;

statements: statement SEMICOLON statements
	      | statement semicolon_string_opt
;

semicolon_string_opt: %empty
                    | SEMICOLON
;

stmt_1: variable ASSIGN expression
	//function_identifier
	  | IDENTIFIER ASSIGN expression
;

identifier_list_1: IDENTIFIER
				   identifier_next
;
identifier_next: %empty
			   | COMMA
				 IDENTIFIER
				 identifier_next
;

field_list: field_list SEMICOLON identifier_list_1 COLON type 
		  | identifier_list_1 COLON type 
;

structured_type_3: RECORD field_list SEMICOLON END
				 | RECORD field_list END
				 | RECORD END
;

type: IDENTIFIER
	| structured_type_3
;

procedure_header: PROCEDURE IDENTIFIER method_parameters_opt SEMICOLON
;
																//scalar_type_identifier
function_header: FUNCTION IDENTIFIER method_parameters_opt COLON IDENTIFIER SEMICOLON
;

method_parameters_opt: %empty
				     | LPAR formal_parameters RPAR
;
												   //both type_identifier
formal_parameters: formal_parameters SEMICOLON var_string_opt identifier_list_1 COLON IDENTIFIER 
			     | var_string_opt identifier_list_1 COLON IDENTIFIER
;

var_string_opt: %empty
			  | VAR
;
%%


namespace yy {

	void mlaskal_parser::error(const location_type& l, const std::string& m)
	{
		message(DUERR_SYNTAX, l, m);
	}

}

