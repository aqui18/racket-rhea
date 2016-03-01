#ifndef RHEA_C_INTERFACE
#define RHEA_C_INTERFACE

#include "simplex_solver.hpp"

#if defined(_MSC_VER)
//  Microsoft
#define EXPORT __declspec(dllexport)
#define IMPORT __declspec(dllimport)
#elif defined(__GNUC__)
#define EXPORT __attribute__((visibility("default")))
#define IMPORT
#else
#define EXPORT
#define IMPORT
#pragma warning Unknown dynamic link import/export semantics.
#endif

extern "C" {
    namespace rhea
    {

        // solver
        EXPORT simplex_solver* solver_new() {
            simplex_solver* s = new simplex_solver();
            s->set_autosolve(false);
            return s;
        }

        EXPORT void solver_delete(simplex_solver* v) {
            delete v;
        }

        EXPORT void solver_add_constraint(solver* s, constraint* c) {
            s->add_constraint(*c);
        }

        EXPORT void solver_remove_constraint(solver* s, constraint* c) {
            s->remove_constraint(*c);
        }

        EXPORT int solver_suggest(simplex_solver* s, variable* v, double x) {
            try {
                s->suggest(*v, x);
                return 0;
            } catch (...) {
                return 1;
            }
        }

        EXPORT void solver_add_edit_var(simplex_solver* s, variable* v) {
            s->add_edit_var(*v);
        }

        EXPORT void solver_begin_edit(simplex_solver* s) {
            s->begin_edit();
        }

        EXPORT void solver_edit_value(simplex_solver* s, variable* var, double v) {
            s->suggest_value(*var, v);
        }

        EXPORT int solver_resolve(simplex_solver* s) {
            try {
                s->resolve();
                return 0;
            } catch (...) {
                return 1;
            }
        }

        EXPORT int solver_end_edit(simplex_solver* s) {
            try {
                s->end_edit();
                return 0;
            } catch (...) {
                return 1;
            }
        }

        EXPORT int solver_solve(solver* s) {
            try {
                s->solve();
                return 0;
            } catch (...) {
                return 1;
            }
        }

        // variables
        EXPORT variable* variable_new(double value) {
            return new variable(value);
        }

        EXPORT stay_constraint* variable_stay(variable* v) {
            return new stay_constraint(*v);
        }

        EXPORT linear_expression* variable_expression(variable* v) {
            return new linear_expression(*v, 1, 0);
        }

        // expressions
        EXPORT double expression_value(linear_expression* v) {
            return v->evaluate();
        }

#define OP(name, op)							\
        EXPORT linear_expression* expression_ ## name                   \
        (linear_expression* e, linear_expression* e2) {                 \
            try {                                                       \
                *e op ## = *e2;                                         \
                return e;                                               \
            } catch (...) {                                             \
                return NULL;                                            \
            }                                                           \
        }                                                               \
        EXPORT linear_expression* expression_ ## name ## _double        \
        (linear_expression* e, double v) {                              \
            *e op ## = linear_expression(v);                            \
            return e;							\
        }
        OP(plus, +)
        OP(minus, -)
        OP(times, *)
        OP(divide, /)

        // inequality
#define COMP(name, rel)                                                 \
        EXPORT constraint* expression_ ## name                          \
        (linear_expression* le1, linear_expression* le2) {              \
            return new constraint(linear_inequality(*le1, rel, *le2));  \
        }                                                               \
        EXPORT constraint* expression_ ## name ## _double               \
        (linear_expression* le1, double v) {                            \
            linear_expression le2 = linear_expression(v);               \
            return new constraint(linear_inequality(*le1, rel, le2));   \
        }
        COMP(leq, relation::leq)
        COMP(geq, relation::geq)

        // equation
        EXPORT constraint* expression_equals
        (linear_expression* le1, linear_expression* le2) {
            return new constraint(linear_equation(*le1, *le2));
        }
        EXPORT constraint* expression_equals_double
        (linear_expression* le1, double v) {
            linear_expression le2 = linear_expression(v);
            return new constraint(linear_equation(*le1, le2));
        }

        // constraints
        EXPORT void constraint_change_strength(linear_constraint* c, strength* str) {
            c->change_strength(*str);
        }

        // strengths
#define MAKE_STRENGTH(name, a, b, c)					\
        static strength* g_strength_ ## name = new strength(a,b,c);     \
        EXPORT strength* strength_ ## name () {                         \
            return g_strength_ ## name;                                 \
        }
        MAKE_STRENGTH(required,
                      std::numeric_limits<double>::max(),
                      std::numeric_limits<double>::max(),
                      std::numeric_limits<double>::max())
        MAKE_STRENGTH(strongest, 100, 0, 0)
        MAKE_STRENGTH(stronger, 10, 0, 0)
        MAKE_STRENGTH(strong, 1, 0, 0)
        MAKE_STRENGTH(medium, 0, 1, 0)
        MAKE_STRENGTH(weak, 0, 0, 100)
        MAKE_STRENGTH(weaker, 0, 0, 10)
        MAKE_STRENGTH(weakest, 0, 0, 1)
    }
}

#endif
