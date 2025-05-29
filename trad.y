
%{                          // SECCION 1 Declaraciones de C-Yacc

#include <stdio.h>
#include <ctype.h>            // declaraciones para tolower
#include <string.h>           // declaraciones para cadenas
#include <stdlib.h>           // declaraciones para exit ()

#define FF fflush(stdout);    // para forzar la impresion inmediata

int yylex () ;
int yyerror () ;
char *mi_malloc (int) ;
char *gen_code (char *) ;
char *int_to_string (int) ;
char *char_to_string (char) ;

// Linked lists for the local variables and functions that contain them

typedef struct s_local_vars { 
    char *var_name ;
    struct s_local_vars *next;
} t_local_vars ;


typedef struct s_functions {
    char *func_name;
    t_local_vars *local_vars;
    struct s_functions *next;
} t_functions;

// Declaration of the functions for local variables implementation

int append_locals(char *func_name, char *locals);
int search_locals(char *func_name, char *var);

// Self-declared variables

char temp [2048] ;
int print_arg_count = 0;
int in_if_else = 0;
char current_function_name[256];
int print_args_counter = 0;

// Definitions for explicit attributes

typedef struct s_attr {
    int value ;    // - Numeric value of a NUMBER 
    char *code ;   // - to pass IDENTIFIER names, and other translations 
} t_attr ;

#define YYSTYPE t_attr

%}

// Token Definitions

%token NUMBER        
%token IDENTIF       // Identificador=variable
%token INTEGER       // identifica el tipo entero
%token STRING
%token MAIN          // identifica el comienzo del proc. main
%token WHILE         // identifica el bucle main
%token FOR
%token IF
%token ELSE
%token PUTS
%token PRINTF
%token EQ
%token AND
%token OR
%token NOT_EQ
%token LESS_EQ
%token MORE_EQ
%token RETURN

// Precedency 

%right '='                    // es la ultima operacion que se debe realizar
%left OR
%left AND
%left NOT_EQ EQ
%left '<' '>' LESS_EQ MORE_EQ
%left '+' '-'                 // menor orden de precedencia
%left '*' '/' '%'               // orden de precedencia intermedio
%left '!'
%left UNARY_SIGN              // mayor orden de precedencia


%%                            // Seccion 3 Gramatica - Semantico

/* - - - - - - - - - - - - - - - - ROOT - - - - - - - - - - - - - - - - */

var_declr:      var_axioma main_declr			{ printf("%s%s", $1.code, $2.code); }

	  	| main_declr					{ printf("%s", $1.code); }
	    ;

/* - - - - - - - - - - - - - - - - GLOBAL VARIABLES - - - - - - - - - - - - - - - - */

var_axioma:       var_sentencia ';' var_r_axioma	{ if ($3.code != NULL) {sprintf (temp,"%s\n%s", $1.code, $3.code) ;} 	
							  else {sprintf (temp,"%s\n", $1.code);} $$.code = gen_code (temp) ; }
            ;



var_r_axioma:						{ $$.code = gen_code(""); }
            	| var_axioma				{ $$.code = $1.code; }
            ;



var_sentencia:   INTEGER IDENTIF n_declaration		{ sprintf (temp, "(setq %s 0)%s", $2.code, $3.code) ; 
                                                   	  $$.code = gen_code (temp); }

	    	| INTEGER IDENTIF '=' NUMBER n_declaration	{ sprintf (temp, "(setq %s %d)%s", $2.code, $4.value, $5.code) ; 
							  	  $$.code = gen_code (temp); }
	    
	   	| INTEGER IDENTIF '[' NUMBER ']' 		{ sprintf (temp, "(setq %s (make-array %d))", $2.code, $4.value) ; 
							  	  $$.code = gen_code (temp) ; }
	   
	    ;



n_declaration:						{ $$.code = gen_code(""); }
    		| ',' IDENTIF n_declaration		{ sprintf(temp, "\n(setq %s 0)%s", $2.code, $3.code); 
						      	  $$.code = gen_code(temp); }
    		| ',' IDENTIF '=' NUMBER n_declaration	{ sprintf(temp, "\n(setq %s %d)%s", $2.code, $4.value, $5.code); 
							 $$.code = gen_code(temp); }
	    ;
	    
/* - - - - - - - - - - - - - - - - MAIN - - - - - - - - - - - - - - - - */

main_declr:	func MAIN {sprintf(current_function_name, "main");} '(' func_args ')' '{'axioma'}' {sprintf (temp,"%s(defun main(%s)\n%s)",$1.code ,$5.code, $8.code);
												   $$.code = gen_code (temp); }
	    ;

/* - - - - - - - - - - - - - - - - FUNCTIONS - - - - - - - - - - - - - - - - */

func:           IDENTIF {sprintf(current_function_name, "%s", $1.code);} '(' func_args ')' '{' axioma '}' func      { sprintf (temp,"\n(defun %s(%s)\n%s)\n%s\n", $1.code,$4.code ,$7.code, $9.code); 
													              $$.code = gen_code (temp) ;} 
					       
	    	| {$$.code = gen_code("");}

	    ;



func_args:    INTEGER IDENTIF r_func_args 	{sprintf(temp, "%s%s", $2.code, $3.code); 
						 $$.code = gen_code(temp);}

		| {$$.code = gen_code("");}
	   ;



r_func_args: ',' INTEGER IDENTIF r_func_args {sprintf(temp, " %s%s", $3.code, $4.code); 
					      $$.code = gen_code(temp);}

		| {$$.code = gen_code("");}
	   ;



axioma:      return_no_end sentencia r_axioma    { sprintf (temp, "%s\t%s\n%s", $1.code, $2.code, $3.code) ; 
						   $$.code = gen_code (temp) ;}
	    				    
	   ;               

/* - - - - - - - - - - - - - - - - AXIOM - - - - - - - - - - - - - - - - */

r_axioma:  
        	        { $$.code = gen_code(""); }

        | axioma        { $$.code = $1.code; }

        | return_end 	{$$.code = $1.code;}

	   ;

/* - - - - - - - - - - - - - - - - RETURN FROM FUNCTION - - - - - - - - - - - - - - - - */

return_end:    RETURN expresion ';' {sprintf (temp, "\t%s", $2.code); 
				     $$.code = gen_code(temp);}

	   ;



return_no_end:	  
				{ $$.code = gen_code(""); }
	|   RETURN expresion ';' {sprintf(temp, "\t(return-from %s %s)", current_function_name, $2.code);
				  $$.code = gen_code (temp);}     

           ;

/* - - - - - - - - - - - - - - - - POSSIBLE INPUTS - - - - - - - - - - - - - - - - */

sentencia:  
	     IDENTIF '=' expresion ';' { 
					 if (search_locals(current_function_name, $1.code) == 0) {
					   	 sprintf (temp, "(setf %s %s)", $1.code, $3.code);
					 } else {sprintf (temp, "(setf %s_%s %s)", current_function_name, $1.code, $3.code);}
                                           	 $$.code = gen_code (temp);
					 }

	    |  INTEGER IDENTIF '=' NUMBER n_declaration ';' { 
								append_locals(current_function_name, $2.code);
					   			sprintf (temp, "(setq %s_%s %d)%s", current_function_name, $2.code, $4.value, $5.code); 
                                           			$$.code = gen_code (temp);
							    }
                                          
            | PUTS '(' STRING ')' ';'      { sprintf (temp, "(print \"%s\")", $3.code);  
                                               $$.code = gen_code (temp); 
					     }
                                           
	    | PRINTF '(' STRING ',' print_multiple_args')' ';'         { if (in_if_else == 1) {
										if (print_args_counter > 0) {
        										sprintf(temp, "(progn\t%s\n\t)", $5.code);
    										} else { sprintf (temp, "%s", $5.code); }
									    } else { sprintf (temp, "%s", $5.code); }
								
   									 print_args_counter = 0;
    									$$.code = gen_code(temp); }  
 
	    | WHILE '('condition')''{'axioma'}' { sprintf(temp, "(loop while %s do \n%s\t)",  $3.code, $6.code); 
	    					  $$.code = gen_code (temp);}

	    | FOR '(' IDENTIF '=' NUMBER ';' for_condition ';'  IDENTIF '=' expresion ')''{'r_axioma'}' {  if (search_locals(current_function_name, $9.code) == 0) {

								sprintf(temp, "(setf %s %d)\n(loop while %s do \n%s \t(setf %s %s))\n",$3.code, $5.value, $7.code, $14.code, $9.code, $11.code);
								} else {
								 sprintf(temp, "(setf %s_%s %d)\n(loop while %s do \n%s \t(setf %s_%s %s))\n", current_function_name, $3.code, $5.value, $7.code, $14.code, current_function_name,$9.code, $11.code);}
	    					   							  $$.code = gen_code (temp); }

	    | IF {in_if_else = 1;} '('condition')''{'progn'}' else_sent { sprintf(temp, "(if %s\n%s\t%s\n\t)",  $4.code, $7.code, $9.code); in_if_else = 0;
	    					    			  $$.code = gen_code (temp) ;} 

  	    | INTEGER IDENTIF '[' NUMBER ']' ';' { append_locals(current_function_name, $2.code); 
						   sprintf (temp, "(setq %s_%s (make-array %d))\n", current_function_name, $2.code, $4.value) ; 
                                                   $$.code = gen_code (temp); }
	
	    | IDENTIF '[' expresion ']' '=' expresion ';'    { if (search_locals(current_function_name, $1.code) == 0) {
					   		  	sprintf (temp, "(setf (aref %s %s) %s)", $1.code, $3.code, $6.code);} else {
								sprintf (temp, "(setf (aref %s_%s %s) %s)", current_function_name, $1.code, $3.code, $6.code);
								}
                                           		        $$.code = gen_code (temp); }

	    | INTEGER IDENTIF n_declaration ';'{ append_locals(current_function_name, $2.code); 
						 sprintf (temp, "(setq %s_%s 0)\n%s", current_function_name, $2.code, $3.code); 
						 $$.code = gen_code (temp); } 

	    | IDENTIF '(' func_call_args ')' ';' { sprintf (temp, "(%s %s)\n", $1.code, $3.code); 
                                               	   $$.code = gen_code (temp); }
	    
	    ;

/* - - - - - - - - - - - - - - - - DECLARATION OF ARGUMENTS FOR FUNCTION CALLS - - - - - - - - - - - - - - - - */

func_call_args:  expresion r_func_call_args 	{ if (search_locals(current_function_name, $1.code) == 0) {
							sprintf(temp, "%s%s", $1.code, $2.code);
						} else {
							sprintf(temp, "%s_%s%s", current_function_name, $1.code, $2.code);
						}
				 		$$.code = gen_code(temp);}

|			      			{$$.code = gen_code("");}

	    ;



r_func_call_args: ',' expresion r_func_call_args {sprintf(temp, " %s%s", $2.code, $3.code);
						  $$.code = gen_code(temp);}
| 		 {$$.code = gen_code("");}

	    ;

/* - - - - - - - - - - - - - - - - FOR LOOP LOGICAL CONDITION- - - - - - - - - - - - - - - - */

for_condition:       IDENTIF EQ expresion  { if (search_locals(current_function_name, $1.code) == 0) {
							sprintf (temp, "(= %s %s)", $1.code, $3.code) ;
						} else {
							sprintf (temp, "(= %s_%s %s)", current_function_name , $1.code, $3.code) ;
						}
                                             $$.code = gen_code (temp) ; }

		 |   IDENTIF NOT_EQ expresion  { if (search_locals(current_function_name, $1.code) == 0) {
						 	sprintf (temp, "(/= %s %s)", $1.code, $3.code) ; 
						} else {
							sprintf (temp, "(/= %s_%s %s)", current_function_name , $1.code, $3.code) ;
						}
                                                 $$.code = gen_code (temp) ; }

		 |   IDENTIF '<' expresion  {  if (search_locals(current_function_name, $1.code) == 0) {
							sprintf (temp, "(< %s %s)", $1.code, $3.code) ;
						} else {
							sprintf (temp, "(< %s_%s %s)", current_function_name , $1.code, $3.code) ;
						}
                                              $$.code = gen_code (temp) ; }

		 |   IDENTIF LESS_EQ expresion  { if (search_locals(current_function_name, $1.code) == 0) {
							sprintf (temp, "(<= %s %s)", $1.code, $3.code) ;
						 } else {
							sprintf (temp, "(<= %s_%s %s)", current_function_name , $1.code, $3.code) ;
						 }
                                          	  $$.code = gen_code (temp) ; }

		 |   IDENTIF '>' expresion  { if (search_locals(current_function_name, $1.code) == 0) {
							sprintf (temp, "(> %s %s)", $1.code, $3.code) ;
						} else {
							sprintf (temp, "(> %s_%s %s)", current_function_name , $1.code, $3.code) ;
						}
                                              $$.code = gen_code (temp) ; }

		 |   IDENTIF MORE_EQ expresion  { if (search_locals(current_function_name, $1.code) == 0) {
							sprintf (temp, "(>= %s %s)", $1.code, $3.code) ;
						} else {
							sprintf (temp, "(>= %s_%s %s)", current_function_name , $1.code, $3.code) ;
						}
                                              $$.code = gen_code (temp) ;}
	    ;

/* - - - - - - - - - - - - - - - - IF AND WHILE LOGICAL CONDITIONS - - - - - - - - - - - - - - - - */

condition:	 |   condition EQ condition	{ sprintf (temp, "(= %s %s)", $1.code, $3.code) ;
                                           		$$.code = gen_code (temp) ; }
		 |   condition AND condition  	{ sprintf (temp, "(and %s %s)", $1.code, $3.code) ;
                                           		$$.code = gen_code (temp) ; }
		 |   condition OR condition  	{ sprintf (temp, "(or %s %s)", $1.code, $3.code) ;
                                           		$$.code = gen_code (temp) ; }
		 |   condition '!' condition  	{ sprintf (temp, "(not %s %s)", $1.code, $3.code) ;
                                           		$$.code = gen_code (temp) ; }
		 |   condition NOT_EQ condition 	{ sprintf (temp, "(/= %s %s)", $1.code, $3.code) ;
                                           		$$.code = gen_code (temp) ; }
		 |   condition '<' condition  	{ sprintf (temp, "(< %s %s)", $1.code, $3.code) ;
                                           		$$.code = gen_code (temp) ; }
		 |   condition LESS_EQ condition 	{ sprintf (temp, "(<= %s %s)", $1.code, $3.code) ;
                                           		$$.code = gen_code (temp) ; }
		 |   condition '>' condition  	{ sprintf (temp, "(> %s %s)", $1.code, $3.code) ;
                                           		$$.code = gen_code (temp) ; }
		 |   condition MORE_EQ condition  { sprintf (temp, "(>= %s %s)", $1.code, $3.code) ;
                                           		$$.code = gen_code (temp) ; }
		 |   condition '%' condition  	{ sprintf (temp, "(mod %s %s)", $1.code, $3.code) ;
                                           		$$.code = gen_code (temp) ; }
                 |   termino                  { if (search_locals(current_function_name, $1.code) == 0) {
							sprintf(temp, "%s", $1.code);
						} else {
							sprintf(temp, "%s_%s", current_function_name, $1.code);
						}
						$$.code = gen_code(temp);}
                 
                 |   IDENTIF '(' func_call_args ')'  { sprintf (temp, "(%s %s)", $1.code, $3.code) ; 
                                               $$.code = gen_code (temp) ;}
                 
	    ;

/* - - - - - - - - - - - - - - - - PRINT MULTIPLE ARGUMENTS - - - - - - - - - - - - - - - - */

print_multiple_args:
		     expresion { sprintf (temp, "(princ %s)", $1.code) ;  
                 		           $$.code = gen_code (temp) ; }

		    | STRING { sprintf (temp, "(princ \"%s\")", $1.code) ;  
                 		           $$.code = gen_code (temp) ; }  

		    | expresion ',' print_multiple_args { print_args_counter += 1; 
							  sprintf (temp, "\n\t(princ %s)\n%s", $1.code, $3.code) ;  
                 		          		  $$.code = gen_code (temp) ; }  

		    | STRING ',' print_multiple_args { print_args_counter += 1; 
						       sprintf (temp, "\n\t(princ \"%s\")\n%s", $1.code, $3.code) ;  
                 		           	       $$.code = gen_code (temp) ; } 
	    ;

/* - - - - - - - - - - - - - - - - ELSE BRANCH - - - - - - - - - - - - - - - - */

else_sent:  ELSE {in_if_else = 1;} '{'progn'}' {sprintf(temp, "\n%s",  $4.code); 
						in_if_else = 0;
	    					$$.code = gen_code (temp) ;}

	    |   { $$.code = gen_code("");  }

	    ;

/* - - - - - - - - - - - - - - - - PROGN FOR IF AND ELSE - - - - - - - - - - - - - - - - */

progn:       sentencia {  sprintf(temp, "\t%s",  $1.code);
	    	          $$.code = gen_code (temp) ;} 

	    | sentencia progn_aux r_axioma { sprintf(temp, "\t(progn\n\t%s\n\t%s\n%s\t)",  $1.code, $2.code, $3.code);
	    				     $$.code = gen_code (temp) ;} 

	    | RETURN expresion ';' {sprintf(temp, "\t(return-from %s %s)", current_function_name, $2.code); 
				    $$.code = gen_code (temp);}   

	    ;
	    


progn_aux: 
	      sentencia { sprintf(temp, "\t%s", $1.code);
			  $$.code = gen_code(temp);}
	      
	    | RETURN expresion ';' {sprintf(temp, "\t(return-from %s %s)", current_function_name, $2.code); 
				    $$.code = gen_code (temp);}   

	    ;

/* - - - - - - - - - - - - - - - - EXPRESSIONS - - - - - - - - - - - - - - - - */

expresion:      termino                  { $$ = $1; }

	    |	IDENTIF '(' func_call_args ')'  { sprintf (temp, "(%s %s)", $1.code, $3.code) ; 
                                                  $$.code = gen_code (temp) ;}

            |   expresion '+' expresion  { sprintf (temp, "(+ %s %s)", $1.code, $3.code) ;
                                           $$.code = gen_code (temp) ; }

            |   expresion '-' expresion  { sprintf (temp, "(- %s %s)", $1.code, $3.code) ;
                                           $$.code = gen_code (temp) ; }

            |   expresion '*' expresion  { sprintf (temp, "(* %s %s)", $1.code, $3.code) ;
                                           $$.code = gen_code (temp) ; }

            |   expresion '/' expresion  { sprintf (temp, "(/ %s %s)", $1.code, $3.code) ;
                                           $$.code = gen_code (temp) ; }

	    |   expresion '%' expresion  { sprintf (temp, "(mod %s %s)", $1.code, $3.code) ;
   					    $$.code = gen_code (temp) ; }

            ;

/* - - - - - - - - - - - - - - - - TERMS - - - - - - - - - - - - - - - - */

termino:        operando                           { $$ = $1 ; }    
                      
            |   '+' operando %prec UNARY_SIGN      { $$ = $1 ; }

            |   '-' operando %prec UNARY_SIGN      { sprintf (temp, "(- %s)", $2.code) ;
                                                     $$.code = gen_code (temp) ; }  
  
            ;

/* - - - - - - - - - - - - - - - - OPERANDS - - - - - - - - - - - - - - - - */

operando:      IDENTIF '[' expresion ']'  { if (search_locals(current_function_name, $1.code) == 0) {sprintf (temp, "(aref %s %s)", $1.code, $3.code) ;} else { sprintf (temp, "(aref %s_%s %s)", current_function_name , $1.code, $3.code) ;}
                                           		$$.code = gen_code (temp) ;  }
	    |   IDENTIF                  { if (search_locals(current_function_name, $1.code) == 0) { sprintf (temp, "%s", $1.code) ; }
					   else { sprintf (temp, "%s_%s", current_function_name, $1.code) ;}
                                           $$.code = gen_code (temp) ; }

            |   NUMBER                   { sprintf (temp, "%d", $1.value) ;
                                           $$.code = gen_code (temp) ; }

            |   '(' expresion ')'        { $$ = $2 ; }

            ;


%%                            // SECCION 4    Codigo en C

int n_line = 1 ;

int yyerror (mensaje)
char *mensaje ;
{
    fprintf (stderr, "%s en la linea %d\n", mensaje, n_line) ;
    printf ( "\n") ;	// bye
}

char *int_to_string (int n)
{
    sprintf (temp, "%d", n) ;
    return gen_code (temp) ;
}

char *char_to_string (char c)
{
    sprintf (temp, "%c", c) ;
    return gen_code (temp) ;
}

char *my_malloc (int nbytes)       // reserva n bytes de memoria dinamica
{
    char *p ;
    static long int nb = 0;        // sirven para contabilizar la memoria
    static int nv = 0 ;            // solicitada en total

    p = malloc (nbytes) ;
    if (p == NULL) {
        fprintf (stderr, "No queda memoria para %d bytes mas\n", nbytes) ;
        fprintf (stderr, "Reservados %ld bytes en %d llamadas\n", nb, nv) ;
        exit (0) ;
    }
    nb += (long) nbytes ;
    nv++ ;

    return p ;
}


/***************************************************************************/
/********************** Seccion of Local Variables Population *********************/
/***************************************************************************/


t_functions *s_functions = NULL;

int search_locals(char *func_name, char *var)
{
    // searches for a var name in each function, if was declared in some function -> returns true
    // if variable or function were not found -> returns false
    t_functions *func = s_functions;

    
    while (func != NULL) {
        if (strcmp(func->func_name, func_name) == 0) {
            t_local_vars *local = func->local_vars;

            while (local != NULL) {
                if (strcmp(local->var_name, var) == 0) {
                    return 1;
                }
                local = local->next;
            }
            return 0;
        }
        func = func->next;
    }

    return 0;  
}

int append_locals(char *function, char *var) 
{
    // append function's list accordingly if declared new variable in it. returns true on success

    t_functions *func = s_functions;

    // check if function exists until end of the linked list is met (last element is NULL)
    while (func != NULL) {
        if (strcmp(func->func_name, function) == 0)
            break;
        func = func->next;
    }

    // create function if it didnt exist before
    if (func == NULL) {
        func = malloc(sizeof(t_functions));
        func->func_name = strdup(function);
        func->local_vars = NULL;
        func->next = s_functions;
        s_functions = func;
    }

    
    t_local_vars *new_local = malloc(sizeof(t_local_vars));

    // filling up new node
    new_local->var_name = strdup(var); 
    new_local->next = NULL;
    
    // if list of local vars is empty, add as a head
    if (func->local_vars == NULL) {
         func->local_vars = new_local;
    } else {
	// if lost is not empty -> move till the end is reached and paste the node there
         t_local_vars *temp = func->local_vars;
         while (temp->next != NULL) {
              temp = temp->next;
         }
         temp->next = new_local;
    }

    //printf("Appended successfully");
    return 1; 

}

/***************************************************************************/
/********************** Seccion de Palabras Reservadas *********************/
/***************************************************************************/


typedef struct s_keyword { // para las palabras reservadas de C
    char *name ;
    int token ;
} t_keyword ;

t_keyword keywords [] = { // define las palabras reservadas y los
    "main",        MAIN,           // y los token asociados
    "int",         INTEGER,
    "puts",        PUTS,
    "printf",      PRINTF,
    "while",       WHILE,
    "for",	   FOR,
    "if",          IF,
    "else", 	   ELSE,
    "==",          EQ,
    "&&", 	   AND,
    "||", 	   OR,
    "!=",	   NOT_EQ,
    "<=",	   LESS_EQ,
    ">=",	   MORE_EQ,
    "return", 	   RETURN,
    NULL,          0               // para marcar el fin de la tabla
} ;

t_keyword *search_keyword (char *symbol_name)
{                                  // Busca n_s en la tabla de pal. res.
                                   // y devuelve puntero a registro (simbolo)
    int i ;
    t_keyword *sim ;

    i = 0 ;
    sim = keywords ;
    while (sim [i].name != NULL) {
	    if (strcmp (sim [i].name, symbol_name) == 0) {
		                             // strcmp(a, b) devuelve == 0 si a==b
            return &(sim [i]) ;
        }
        i++ ;
    }

    return NULL ;
}

 
/***************************************************************************/
/******************* Seccion del Analizador Lexicografico ******************/
/***************************************************************************/

char *gen_code (char *name)     // copia el argumento a un
{                                      // string en memoria dinamica
    char *p ;
    int l ;
	
    l = strlen (name)+1 ;
    p = (char *) my_malloc (l) ;
    strcpy (p, name) ;
	
    return p ;
}


int yylex ()
{
// NO MODIFICAR ESTA FUNCION SIN PERMISO
    int i ;
    unsigned char c ;
    unsigned char cc ;
    char ops_expandibles [] = "!<=|>%&/+-*" ;
    char temp_str [256] ;
    t_keyword *symbol ;

    do {
        c = getchar () ;

        if (c == '#') {	// Ignora las lineas que empiezan por #  (#define, #include)
            do {		//	OJO que puede funcionar mal si una linea contiene #
                c = getchar () ;
            } while (c != '\n') ;
        }

        if (c == '/') {	// Si la linea contiene un / puede ser inicio de comentario
            cc = getchar () ;
            if (cc != '/') {   // Si el siguiente char es /  es un comentario, pero...
                ungetc (cc, stdin) ;
            } else {
                c = getchar () ;	// ...
                if (c == '@') {	// Si es la secuencia //@  ==> transcribimos la linea
                    do {		// Se trata de codigo inline (Codigo embebido en C)
                        c = getchar () ;
                        putchar (c) ;
                    } while (c != '\n') ;
                } else {		// ==> comentario, ignorar la linea
                    while (c != '\n') {
                        c = getchar () ;
                    }
                }
            }
        } else if (c == '\\') c = getchar () ;
		
        if (c == '\n')
            n_line++ ;

    } while (c == ' ' || c == '\n' || c == 10 || c == 13 || c == '\t') ;

    if (c == '\"') {
        i = 0 ;
        do {
            c = getchar () ;
            temp_str [i++] = c ;
        } while (c != '\"' && i < 255) ;
        if (i == 256) {
            printf ("AVISO: string con mas de 255 caracteres en linea %d\n", n_line) ;
        }		 	// habria que leer hasta el siguiente " , pero, y si falta?
        temp_str [--i] = '\0' ;
        yylval.code = gen_code (temp_str) ;
        return (STRING) ;
    }

    if (c == '.' || (c >= '0' && c <= '9')) {
        ungetc (c, stdin) ;
        scanf ("%d", &yylval.value) ;
//         printf ("\nDEV: NUMBER %d\n", yylval.value) ;        // PARA DEPURAR
        return NUMBER ;
    }

    if ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')) {
        i = 0 ;
        while (((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') ||
            (c >= '0' && c <= '9') || c == '_') && i < 255) {
            temp_str [i++] = tolower (c) ;
            c = getchar () ;
        }
        temp_str [i] = '\0' ;
        ungetc (c, stdin) ;

        yylval.code = gen_code (temp_str) ;
        symbol = search_keyword (yylval.code) ;
        if (symbol == NULL) {    // no es palabra reservada -> identificador antes vrariabre
//               printf ("\nDEV: IDENTIF %s\n", yylval.code) ;    // PARA DEPURAR
            return (IDENTIF) ;
        } else {
//               printf ("\nDEV: OTRO %s\n", yylval.code) ;       // PARA DEPURAR
            return (symbol->token) ;
        }
    }

    if (strchr (ops_expandibles, c) != NULL) { // busca c en ops_expandibles
        cc = getchar () ;
        sprintf (temp_str, "%c%c", (char) c, (char) cc) ;
        symbol = search_keyword (temp_str) ;
        if (symbol == NULL) {
            ungetc (cc, stdin) ;
            yylval.code = NULL ;
            return (c) ;
        } else {
            yylval.code = gen_code (temp_str) ; // aunque no se use
            return (symbol->token) ;
        }
    }

//    printf ("\nDEV: LITERAL %d #%c#\n", (int) c, c) ;      // PARA DEPURAR
    if (c == EOF || c == 255 || c == 26) {
//         printf ("tEOF ") ;                                // PARA DEPURAR
        return (0) ;
    }

    return c ;
}


int main ()
{
    yyparse () ;
}
