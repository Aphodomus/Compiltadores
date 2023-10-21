%{
#include <iostream>
#include "cool-tree.h"
#include "stringtab.h"
#include "utilities.h"

#define YYLTYPE int
#define cool_yylloc curr_lineno
#define YYLLOC_DEFAULT(Current, Rhs, N) Current = Rhs[1]; node_lineno = Current;
#define SET_NODELOC(Current) node_lineno = Current;

extern char *curr_filename;
extern int node_lineno;
extern int yylex();

void yyerror(char *s);


/* ================================================================ DONT CHANGE ANYTHING IN THIS SECTION ================================================================ */

Program ast_root;
Classes parse_results;
int omerrs = 0;
%}

/* A union of all the types that can be the result of parsing actions. */
%union {
  Boolean boolean;
  Symbol symbol;
  Program program;
  Class_ class_;
  Classes classes;
  Feature feature;
  Features features;
  Formal formal;
  Formals formals;
  Case case_;
  Cases cases;
  Expression expression;
  Expressions expressions;
  char *error_msg;
}

%token CLASS 258 ELSE 259 FI 260 IF 261 IN 262 
%token INHERITS 263 LET 264 LOOP 265 POOL 266 THEN 267 WHILE 268
%token CASE 269 ESAC 270 OF 271 DARROW 272 NEW 273 ISVOID 274
%token <symbol>  STR_CONST 275 INT_CONST 276 
%token <boolean> BOOL_CONST 277
%token <symbol>  TYPEID 278 OBJECTID 279 
%token ASSIGN 280 NOT 281 LE 282 ERROR 283

/*  DON'T CHANGE ANYTHING ABOVE THIS LINE, OR YOUR PARSER WONT WORK       */
/**************************************************************************/
 
   /* Complete the nonterminal list below, giving a type for the semantic
      value of each non terminal. (See section 3.6 in the bison 
      documentation for details). */

/* Declare types for the grammar's non-terminals. */
%type <program> program
%type <classes> class_list
%type <class_> class
%type <features> feature_list
%type <feature> feature
%type <feature> method
%type <feature> attribute
%type <formals> formal_list
%type <formal> formal
%type <cases> branch_list
%type <case_> branch
%type <expression> let_expression
%type <expression> static_dispatch_expression
%type <expression> dispatch_expression
%type <expression> expression
%type <expressions> multi_expression
%type <expressions> expression_list

/* Precedence declarations go here. */
%right ASSIGN
%left NOT
%nonassoc '<' '=' LE
%left '+' '-'
%left '*' '/'
%left ISVOID
%left '~'
%left '@'
%left '.'

%%

program: class_list {
  @$ = @1;
  ast_root = program($1);
};

class_list: class {
  $$ = single_Classes($1);
  parse_results = $$;
} | class_list class {
  $$ = append_Classes($1,single_Classes($2)); 
  parse_results = $$;
};

class: CLASS TYPEID '{' feature_list '}' ';' {
  char objectStr[] = "Object";
  $$ = class_($2, idtable.add_string(objectStr), $4, stringtable.add_string(curr_filename));
} | CLASS TYPEID INHERITS TYPEID '{' feature_list '}' ';' {
  $$ = class_($2, $4, $6, stringtable.add_string(curr_filename));
} | error ';' {

}

feature_list: {
  $$ = nil_Features();
} | feature {
  $$ = single_Features($1);
} | feature_list feature {
  $$ = append_Features($1, single_Features($2));
};

method: OBJECTID '(' formal_list ')' ':' TYPEID '{' expression '}' ';' {
  $$ = method($1, $3, $6, $8);
} | OBJECTID '(' ')' ':' TYPEID '{' expression '}' ';' {
  $$ = method($1, nil_Formals(), $5, $7);
};

attribute: OBJECTID ':' TYPEID ASSIGN expression ';' {
  $$ = attr($1, $3, $5);
} | OBJECTID ':' TYPEID ';' {
  $$ = attr($1, $3, no_expr());
};

feature: method {
  $$ = $1;
} | attribute {
  $$ = $1;
} | error ';' {

};

formal_list: formal {
  $$ = single_Formals($1);
} | formal_list ',' formal {
  $$ = append_Formals($1, single_Formals($3));
};

formal: OBJECTID ':' TYPEID {
  $$ = formal($1, $3);
};

branch_list: branch {
  $$ = single_Cases($1);
} | branch_list branch {
  $$ = append_Cases($1, single_Cases($2));
};

branch: OBJECTID ':' TYPEID DARROW expression ';' {
    $$ = branch($1, $3, $5);
};

let_expression: LET OBJECTID ':' TYPEID IN expression {
  $$ = let($2, $4, no_expr(), $6);
} | LET OBJECTID ':' TYPEID ASSIGN expression IN expression {
  $$ = let($2, $4, $6, $8);
} | LET OBJECTID ':' TYPEID let_expression {
  $$ = let($2, $4, no_expr(), $5);
} | LET OBJECTID ':' TYPEID ASSIGN expression let_expression {
  $$ = let($2, $4, $6, $7);
} | ',' OBJECTID ':' TYPEID IN expression {
  $$ = let($2, $4, no_expr(), $6);
} | ',' OBJECTID ':' TYPEID ASSIGN expression IN expression {
  $$ = let($2, $4, $6, $8);
} | ',' OBJECTID ':' TYPEID let_expression {
  $$ = let($2, $4, no_expr(), $5);
} | ',' OBJECTID ':' TYPEID ASSIGN expression let_expression {
  $$ = let($2, $4, $6, $7);
} | LET error IN expression {

};

expression_list:  expression {
  $$ = single_Expressions($1);
} | expression_list ',' expression {
  $$ = append_Expressions($1, single_Expressions($3));
};

multi_expression: expression ';' {
  $$ = single_Expressions($1);
} | multi_expression expression ';' {
  $$ = append_Expressions($1, single_Expressions($2));
} | error ';' {

};

static_dispatch_expression: expression '@' TYPEID '.' OBJECTID '(' ')' {
  $$ = static_dispatch($1, $3, $5, nil_Expressions());
} | expression '@' TYPEID '.' OBJECTID '(' expression_list ')' {
  $$ = static_dispatch($1, $3, $5, $7);
};

dispatch_expression:  expression '.' OBJECTID '(' ')' {
  $$ = dispatch($1, $3, nil_Expressions());
} | expression '.' OBJECTID '(' expression_list ')' {
  $$ = dispatch($1, $3, $5);
} | OBJECTID '(' ')' {
  char objectStr[] = "self";
  $$ = dispatch(object(idtable.add_string(objectStr)), $1, nil_Expressions());
} | OBJECTID '(' expression_list ')' {
  char objectStr[] = "self";
  $$ = dispatch(object(idtable.add_string(objectStr)), $1, $3);
};

expression: OBJECTID ASSIGN expression {
  $$ = assign($1, $3);
} | static_dispatch_expression {
  $$ = $1;
} | dispatch_expression {
  $$ = $1;
} | IF expression THEN expression ELSE expression FI {
  $$ = cond($2, $4, $6);
} | WHILE expression LOOP expression POOL {
  $$ = loop($2, $4);
} | '{' multi_expression '}' {
  $$ = block($2);
} | let_expression {
  $$ = $1;
} | CASE expression OF branch_list ESAC {
  $$ = typcase($2, $4);
} | NEW TYPEID {
  $$ = new_($2);
} | ISVOID expression {
  $$ = isvoid($2);
} | expression '+' expression {
  $$ = plus($1, $3);
} | expression '-' expression {
  $$ = sub($1, $3);
} | expression '*' expression {
  $$ = mul($1, $3);
} | expression '/' expression {
  $$ = divide($1, $3);
} | '~' expression {
  $$ = neg($2);
} | expression '<' expression {
  $$ = lt($1, $3);
} | expression LE expression {
  $$ = leq($1, $3);
} | expression '=' expression {
  $$ = eq($1, $3);
} | NOT expression {
  $$ = comp($2);
} | '(' expression ')' {
  $$ = $2;
} | OBJECTID {
  $$ = object($1);
} | INT_CONST {
  $$ = int_const($1);
} | STR_CONST {
  $$ = string_const($1);
} | BOOL_CONST {
  $$ = bool_const($1);
};

%%

/* This function is called automatically when Bison detects a parse error. */
void yyerror(char *s)
{
  extern int curr_lineno;

  cerr << "\"" << curr_filename << "\", line " << curr_lineno << ": " \
    << s << " at or near ";
  print_cool_token(yychar);
  cerr << endl;
  omerrs++;

  if(omerrs>50) {fprintf(stdout, "More than 50 errors\n"); exit(1);}
}

