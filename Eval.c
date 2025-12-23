#include "Eval.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
/* Eval.c: Code for evaluatiob of the abstract syntax tree
 * Makes heavy use of data types defined in Eval.h. 
 * Most funcions take a list of functions, and enviroment.
 * scope) eval things normally :) vals: see below
 */


//insert a new variable-name/value pair into the enviroment
// all entries allocated on the heap, so must be free'd.
void env_insert(ident_t name, val_t val, env_t **env) {
  env_t* copy = *env;
  //try to update in place
  while(copy != NULL){
    if(strcmp(copy->name,name) == 0){
      copy->val = val;
      return;
    }
    copy = copy->next;
  }

  //can't update in place: make new entry
  env_t *new = malloc(sizeof(env_t));
  new->name = name;
  new->val = val;
  new->next = *env;
  *env = new;
}

//look up a variable in enviroment
//returns a bunk value (with .is_invalid = 1) if not found
val_t env_lookup(ident_t name, env_t *env) {
  while (env) {
    if (strcmp(name, env->name) == 0) {
      return env->val;
    }
    env = env->next;
  }
  return (val_t) {.is_invalid = 1};
}

void free_env(env_t *env) {
  if (env == NULL) {
    return;
  }
  if (env->next == NULL) {
    free(env);
    return;
  }
  env_t *h = env;
  env = env->next;
  while (env != NULL) {
    free(h);
    h = env;
    env = env->next;
  }
  free(h);
}

//looks up a function from the function list
//returns NULL if not found
fundecl_t *lookup_func(ident_t name, func_list_t *funcs) {
  while (funcs != NULL) {
    if (strcmp(name, funcs->fundecl->name) == 0) {
      return funcs->fundecl;
    } else {
      funcs = funcs->next;
    }
  }
  return NULL;
}


//Evaluates a function
//If there are an insufficient amount of arguments or the function has no return value,
//let the user know and return an invalid val_t
val_t eval_func(fundecl_t *fn, expr_t **arg_expr_arr, env_t *arg_env,
                func_list_t *funcs) {
  int arg_ct = 0, param_ct = 0;
  if (arg_expr_arr != NULL){

  
    while (arg_expr_arr[arg_ct] != NULL){
      arg_ct++;
    }
    while(fn->args[param_ct] != NULL){
      param_ct++;
    }
    if (arg_ct < param_ct){
      printf("ERROR: Function %s called with too few arguments!\n", fn->name);
      return (val_t) {.is_invalid = 1};
    }  
  }
  env_t *fn_env = NULL;
  if (arg_expr_arr != NULL){
    for (int i = 0; arg_expr_arr[i] != NULL; i++){
      val_t v = eval_expr(arg_expr_arr[i], arg_env, funcs);
      if (v.is_invalid == 1){
        return (val_t) {.is_invalid = 1};
      }
      env_insert(fn->args[i], v, &fn_env);
    }
  }
  val_t out = (val_t) {.is_invalid = 1};
  int block_success = eval_block(fn->body, &out, &fn_env, funcs);
  free_env(fn_env);
  if (block_success == 0){
    printf("ERROR: Function %s did not return!\n", fn->name);
    return (val_t) {.is_invalid = 1};
  }
  if (out.is_invalid == 1){
    return (val_t) {.is_invalid = 1};
  }
  return out;
}

//Evaluates an expression, returning an invalid val_t on error.
val_t eval_expr(expr_t *e, env_t *env, func_list_t *funcs) {
  if (e->tag == is_int){
    return (val_t) {.val = e->integer, .is_bool = 0, .is_invalid = 0};
  } else if (e->tag == is_true){
    return (val_t) {.val = 1, .is_bool = 1, .is_invalid = 0};
  } else if (e->tag == is_false) {
    return (val_t) {.val = 0, .is_bool = 1, .is_invalid = 0};
  } else if (e->tag == is_var){
    val_t v = env_lookup(e->var, env);
    if (v.is_invalid == 1){
      printf("ERROR: Couldn't find variable %s in scope\n", e->var);
      return (val_t) {.is_invalid = 1};
    }
    return v;
  } else if (e->tag == is_fn_call){
    fundecl_t *func = lookup_func(e->fn_name, funcs);
    if (func == NULL){
      printf("ERROR: Tried to call invalid function %s\n", e->fn_name);
      return (val_t) {.is_invalid = 1};
    }
    val_t v = eval_func(func, e->args, env, funcs);
    return v;
  } else if (e->tag == is_binop) {
    // Recursively evaluate left and right expressions, as they will eventually fall into one of the above cases
    val_t left = eval_expr(e->binop_expr.left_expr, env, funcs);
    if (left.is_invalid == 1){
      return (val_t) {.is_invalid = 1};
    }
    val_t right = eval_expr(e->binop_expr.right_expr, env, funcs);
    if (right.is_invalid == 1){
      return (val_t) {.is_invalid = 1};
    }
    // We have to compare the expressions differently based on e->binop_expr.tag
    switch (e->binop_expr.tag){
      case is_add:
        return (val_t) {.val = left.val + right.val, .is_bool = 0, .is_invalid = 0};
        break;
      case is_sub:
        return (val_t) {.val = left.val - right.val, .is_bool = 0, .is_invalid = 0};
        break;
      case is_mul:
        return (val_t) {.val = left.val * right.val, .is_bool = 0, .is_invalid = 0};
        break;
      case is_div:
        return (val_t) {.val = left.val / right.val, .is_bool = 0, .is_invalid = 0};
        break;
      case is_lte:
        return (val_t) {.val = left.val <= right.val, .is_bool = 1, .is_invalid = 0};
        break;
      case is_eq:
        return (val_t) {.val = left.val == right.val, .is_bool = 1, .is_invalid = 0};
        break;
      case is_neq:
        return (val_t) {.val = left.val != right.val, .is_bool = 1, .is_invalid = 0};
        break;
      case is_gte:
        return (val_t) {.val = left.val >= right.val, .is_bool = 1, .is_invalid = 0};
        break;
      case is_lt:
        return (val_t) {.val = left.val < right.val, .is_bool = 1, .is_invalid = 0};
        break;
      case is_gt:
        return (val_t) {.val = left.val > right.val, .is_bool = 1, .is_invalid = 0};
        break;
      case is_and:
        return (val_t) {.val = left.val && right.val, .is_bool = 1, .is_invalid = 0};
        break;
      case is_or:
        return (val_t) {.val = left.val || right.val, .is_bool = 1, .is_invalid = 0};
        break;
      default:
	return (val_t) {.is_invalid = 1};
    }
  }
  else { return (val_t) {.is_invalid = 1}; }
}

//Evalutates a statement.
//returns 1 if errors or returns, 0 otherwise
int eval_statement(statement_t *s, val_t *ret_val, env_t **env,
                   func_list_t *funcs) {
  if (s->tag == is_assn){
    //Insert / update variable in enviroment
    val_t v = eval_expr(s->expr, *env, funcs);
    if (v.is_invalid == 1){
      return 1;
    }
    env_insert(s->to_assn, v, env);
    return 0;
  } else if (s->tag == is_print){
    //Evaluate print expression and print its value
    val_t v = eval_expr(s->to_print, *env, funcs);
    if (v.is_invalid == 1){
      return 1;
    } else {
      if (v.is_bool == 1){
        if (v.val != 0){
          printf("true\n");
        } else {
          printf("false\n");
        }
      } else {
        printf("%d\n", v.val);
      }
      return 0;
    }
  } else if (s->tag == is_if){
    // Evaluate if condition and execute then block if true
    val_t v = eval_expr(s->if_condition, *env, funcs);
    if (v.is_invalid == 1){
      return 1;
    }
    if (v.is_bool == 1){
      if (v.val != 0){
        return eval_block(s->then_block, ret_val, env, funcs);
      } else {
        return 0;
      }
    } else {
      return 1;
    }
  } else if (s->tag == is_if_else){
    // Evaluate if condition. Execute then block if true, else block if false
    val_t v = eval_expr(s->if_condition, *env, funcs);
    if (v.is_invalid == 1){
      return 1;
    }
    if (v.is_bool == 1){
      if (v.val != 0){
        return eval_block(s->then_block, ret_val, env, funcs);
      } else {
        return eval_block(s->else_block, ret_val, env, funcs);
      }
    } else {
      return 1;
    }
  } else if (s->tag == is_while){
    // Evaluate while condition and continuously execute body while true
    while (1){
      val_t v = eval_expr(s->while_condition, *env, funcs);
      if (v.is_invalid == 1){
        return 1;
      }
      if (v.val == 0){
        break;
      }
      int body = eval_block(s->body, ret_val, env, funcs);
      if (body){
        return 1;
      }
    }
    return 0;
  } else if (s->tag == is_state_expr){
    // Evaluate expression statement
    val_t v = eval_expr(s->expr_state, *env, funcs);
    if (v.is_invalid == 1){
      return 1;
    } else {
      return 0;
    }
  } else if (s->tag == is_return){
    // Evaluate return expression and set ret_val
    val_t v = eval_expr(s->to_return, *env, funcs);
    if (v.is_invalid == 1){
      return 1;
    }
    *ret_val = v;
    return 1;
  }
  return 1;
}

//evaluates a null-terminated block of statements
int eval_block(statement_t **s, val_t *ret_val, env_t **env,
               func_list_t *funcs) {
  // Simply call eval_statement on all the statements.
  // If any eval_statement calls return 1 we return 1 in eval_block.
  for (int i = 0; s[i] != NULL; i++){
    int res = eval_statement(s[i], ret_val, env, funcs);
    if (res){
      return 1;
    }
  }
  return 0;
}
