#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void explicit_SEIR_deterministic_initmod_desolve(void *);
extern void explicit_SEIR_deterministic_output_dde(void *);
extern void explicit_SEIR_deterministic_rhs_dde(void *);
extern void explicit_SEIR_deterministic_rhs_desolve(void *);
extern void explicit_SEIR_rhs_dde(void *);
extern void less_basic_model_for_js_initmod_desolve(void *);
extern void less_basic_model_for_js_rhs_dde(void *);
extern void less_basic_model_for_js_rhs_desolve(void *);
extern void simple_SEIR_rhs_dde(void *);

/* .Call calls */
extern SEXP explicit_SEIR_contents(SEXP);
extern SEXP explicit_SEIR_create(SEXP);
extern SEXP explicit_SEIR_deterministic_contents(SEXP);
extern SEXP explicit_SEIR_deterministic_create(SEXP);
extern SEXP explicit_SEIR_deterministic_initial_conditions(SEXP, SEXP);
extern SEXP explicit_SEIR_deterministic_metadata(SEXP);
extern SEXP explicit_SEIR_deterministic_rhs_r(SEXP, SEXP, SEXP);
extern SEXP explicit_SEIR_deterministic_set_initial(SEXP, SEXP, SEXP, SEXP);
extern SEXP explicit_SEIR_deterministic_set_user(SEXP, SEXP);
extern SEXP explicit_SEIR_initial_conditions(SEXP, SEXP);
extern SEXP explicit_SEIR_metadata(SEXP);
extern SEXP explicit_SEIR_rhs_r(SEXP, SEXP, SEXP);
extern SEXP explicit_SEIR_set_initial(SEXP, SEXP, SEXP, SEXP);
extern SEXP explicit_SEIR_set_user(SEXP, SEXP);
extern SEXP less_basic_model_for_js_contents(SEXP);
extern SEXP less_basic_model_for_js_create(SEXP);
extern SEXP less_basic_model_for_js_initial_conditions(SEXP, SEXP);
extern SEXP less_basic_model_for_js_metadata(SEXP);
extern SEXP less_basic_model_for_js_rhs_r(SEXP, SEXP, SEXP);
extern SEXP less_basic_model_for_js_set_initial(SEXP, SEXP, SEXP, SEXP);
extern SEXP less_basic_model_for_js_set_user(SEXP, SEXP);
extern SEXP simple_SEIR_contents(SEXP);
extern SEXP simple_SEIR_create(SEXP);
extern SEXP simple_SEIR_initial_conditions(SEXP, SEXP);
extern SEXP simple_SEIR_metadata(SEXP);
extern SEXP simple_SEIR_rhs_r(SEXP, SEXP, SEXP);
extern SEXP simple_SEIR_set_initial(SEXP, SEXP, SEXP, SEXP);
extern SEXP simple_SEIR_set_user(SEXP, SEXP);

static const R_CMethodDef CEntries[] = {
    {"explicit_SEIR_deterministic_initmod_desolve", (DL_FUNC) &explicit_SEIR_deterministic_initmod_desolve, 1},
    {"explicit_SEIR_deterministic_output_dde",      (DL_FUNC) &explicit_SEIR_deterministic_output_dde,      1},
    {"explicit_SEIR_deterministic_rhs_dde",         (DL_FUNC) &explicit_SEIR_deterministic_rhs_dde,         1},
    {"explicit_SEIR_deterministic_rhs_desolve",     (DL_FUNC) &explicit_SEIR_deterministic_rhs_desolve,     1},
    {"explicit_SEIR_rhs_dde",                       (DL_FUNC) &explicit_SEIR_rhs_dde,                       1},
    {"less_basic_model_for_js_initmod_desolve",     (DL_FUNC) &less_basic_model_for_js_initmod_desolve,     1},
    {"less_basic_model_for_js_rhs_dde",             (DL_FUNC) &less_basic_model_for_js_rhs_dde,             1},
    {"less_basic_model_for_js_rhs_desolve",         (DL_FUNC) &less_basic_model_for_js_rhs_desolve,         1},
    {"simple_SEIR_rhs_dde",                         (DL_FUNC) &simple_SEIR_rhs_dde,                         1},
    {NULL, NULL, 0}
};

static const R_CallMethodDef CallEntries[] = {
    {"explicit_SEIR_contents",                         (DL_FUNC) &explicit_SEIR_contents,                         1},
    {"explicit_SEIR_create",                           (DL_FUNC) &explicit_SEIR_create,                           1},
    {"explicit_SEIR_deterministic_contents",           (DL_FUNC) &explicit_SEIR_deterministic_contents,           1},
    {"explicit_SEIR_deterministic_create",             (DL_FUNC) &explicit_SEIR_deterministic_create,             1},
    {"explicit_SEIR_deterministic_initial_conditions", (DL_FUNC) &explicit_SEIR_deterministic_initial_conditions, 2},
    {"explicit_SEIR_deterministic_metadata",           (DL_FUNC) &explicit_SEIR_deterministic_metadata,           1},
    {"explicit_SEIR_deterministic_rhs_r",              (DL_FUNC) &explicit_SEIR_deterministic_rhs_r,              3},
    {"explicit_SEIR_deterministic_set_initial",        (DL_FUNC) &explicit_SEIR_deterministic_set_initial,        4},
    {"explicit_SEIR_deterministic_set_user",           (DL_FUNC) &explicit_SEIR_deterministic_set_user,           2},
    {"explicit_SEIR_initial_conditions",               (DL_FUNC) &explicit_SEIR_initial_conditions,               2},
    {"explicit_SEIR_metadata",                         (DL_FUNC) &explicit_SEIR_metadata,                         1},
    {"explicit_SEIR_rhs_r",                            (DL_FUNC) &explicit_SEIR_rhs_r,                            3},
    {"explicit_SEIR_set_initial",                      (DL_FUNC) &explicit_SEIR_set_initial,                      4},
    {"explicit_SEIR_set_user",                         (DL_FUNC) &explicit_SEIR_set_user,                         2},
    {"less_basic_model_for_js_contents",               (DL_FUNC) &less_basic_model_for_js_contents,               1},
    {"less_basic_model_for_js_create",                 (DL_FUNC) &less_basic_model_for_js_create,                 1},
    {"less_basic_model_for_js_initial_conditions",     (DL_FUNC) &less_basic_model_for_js_initial_conditions,     2},
    {"less_basic_model_for_js_metadata",               (DL_FUNC) &less_basic_model_for_js_metadata,               1},
    {"less_basic_model_for_js_rhs_r",                  (DL_FUNC) &less_basic_model_for_js_rhs_r,                  3},
    {"less_basic_model_for_js_set_initial",            (DL_FUNC) &less_basic_model_for_js_set_initial,            4},
    {"less_basic_model_for_js_set_user",               (DL_FUNC) &less_basic_model_for_js_set_user,               2},
    {"simple_SEIR_contents",                           (DL_FUNC) &simple_SEIR_contents,                           1},
    {"simple_SEIR_create",                             (DL_FUNC) &simple_SEIR_create,                             1},
    {"simple_SEIR_initial_conditions",                 (DL_FUNC) &simple_SEIR_initial_conditions,                 2},
    {"simple_SEIR_metadata",                           (DL_FUNC) &simple_SEIR_metadata,                           1},
    {"simple_SEIR_rhs_r",                              (DL_FUNC) &simple_SEIR_rhs_r,                              3},
    {"simple_SEIR_set_initial",                        (DL_FUNC) &simple_SEIR_set_initial,                        4},
    {"simple_SEIR_set_user",                           (DL_FUNC) &simple_SEIR_set_user,                           2},
    {NULL, NULL, 0}
};

void R_init_squire(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
