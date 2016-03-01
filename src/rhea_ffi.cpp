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
        EXPORT void* solver_new() {
            simplex_solver* s = new simplex_solver();
            s->set_autosolve(false);
            return reinterpret_cast<void*>(s);
        }

        EXPORT void solver_delete(void* v) {
            simplex_solver* s = reinterpret_cast<simplex_solver*>(v);
            delete s;
        }

        EXPORT void solver_add_constraint(void* s, void* c) {
            (reinterpret_cast<solver*>(s))->
                add_constraint(*reinterpret_cast<constraint*>(c));
        }

        EXPORT int solver_suggest(void* s, void* v, double x) {
            try {
                (reinterpret_cast<simplex_solver*>(s))->
                    suggest(*reinterpret_cast<variable*>(v), x);
                return 0;
            } catch (...) {
                return 1;
            }
        }

        EXPORT void solver_add_edit_var(void* s, void* v) {
            reinterpret_cast<simplex_solver*>(s)->
                add_edit_var(*reinterpret_cast<variable*>(v));
        }

        EXPORT void solver_begin_edit(void* s) {
            reinterpret_cast<simplex_solver*>(s)->begin_edit();
        }

        EXPORT void solver_edit_value(void* s, void* var, double v) {
            reinterpret_cast<simplex_solver*>(s)->
                suggest_value(*reinterpret_cast<variable*>(var), v);
        }

        EXPORT int solver_resolve(void* s, void* var, double v) {
            try {
                reinterpret_cast<simplex_solver*>(s)->resolve();
                return 0;
            } catch (...) {
                return 1;
            }
        }

        EXPORT void solver_end_edit(void* s) {
            reinterpret_cast<simplex_solver*>(s)->end_edit();
        }

        EXPORT int solver_solve(void* s) {
            try {
                reinterpret_cast<solver*>(s)->solve();
                return 0;
            } catch (...) {
                return 1;
            }
        }

        // variables
        EXPORT void* variable_new(double value) {
            variable v(value);
            linear_expression* e = new linear_expression(v, 1, 0);
            return reinterpret_cast<void*>(e);
        }

        EXPORT void* variable_stay(void* v) {
            variable* var = reinterpret_cast<variable*>(v);
            stay_constraint* s = new stay_constraint(*var);
            return reinterpret_cast<void*>(s);
        }

        // expressions
        EXPORT double expression_value(void* v) {
            return reinterpret_cast<linear_expression*>(v)->evaluate();
        }

#define OP(name, op)							\
        EXPORT void* expression_ ## name (void* e, void* e2) {		\
            try {                                                       \
                *reinterpret_cast<linear_expression*>(e) op ## =  \
                    *reinterpret_cast<linear_expression*>(e2);    \
            } catch (...) {                                             \
                return NULL;                                            \
            }                                                           \
            return e;							\
        }                                                               \
        EXPORT void* expression_ ## name ## _double(void* e, double v) { \
            linear_expression e2(v);                              \
            *reinterpret_cast<linear_expression*>(e) op ## = e2;	\
            return e;							\
        }
        OP(plus, +)
        OP(minus, -)
        OP(times, *)
        OP(divide, /)

        // inequality
#define COMP(name, rel)                                                 \
        EXPORT void* expression_ ## name (void* e1, void* e2) {		\
            linear_expression le1 = *reinterpret_cast<linear_expression*>(e1); \
            linear_expression le2 = *reinterpret_cast<linear_expression*>(e2); \
            constraint* c = new constraint(linear_inequality(le1, rel, le2)); \
            return reinterpret_cast<void*>(c);                          \
        }                                                               \
        EXPORT void* expression_ ## name ## _double(void* e1, double v) { \
            linear_expression le1 = *reinterpret_cast<linear_expression*>(e1); \
            linear_expression le2 = linear_expression(v);               \
            constraint* c = new constraint(linear_inequality(le1, rel, le2)); \
            return reinterpret_cast<void*>(c);				\
        }
        COMP(leq, relation::leq)
        COMP(geq, relation::geq)

        // equation
        EXPORT void* expression_equals(void* e1, void* e2) {
            linear_expression le1 = *reinterpret_cast<linear_expression*>(e1);
            linear_expression le2 = *reinterpret_cast<linear_expression*>(e2);
            constraint* c = new constraint(linear_equation(le1, le2));
            return reinterpret_cast<void*>(c);
        }
        EXPORT void* expression_equals_double(void* e1, double v) {
            linear_expression le1 = *reinterpret_cast<linear_expression*>(e1);
            linear_expression le2 = linear_expression(v);
            constraint* c = new constraint(linear_equation(le1, le2));
            return reinterpret_cast<void*>(c);
        }

        // constraints
        EXPORT void constriant_change_strength(void* c, void* str) {
            reinterpret_cast<linear_constraint*>(c)->
                change_strength(*reinterpret_cast<strength*>(str));
        }

        // strengths
#define MAKE_STRENGTH(name, a, b, c)					\
        EXPORT void* strength_ ## name () {                             \
            return reinterpret_cast<void*>(new strength(a,b,c));	\
        }
        MAKE_STRENGTH(required,
                      std::numeric_limits<double>::max(),
                      std::numeric_limits<double>::max(),
                      std::numeric_limits<double>::max())
        MAKE_STRENGTH(strong, 1, 0, 0)
        MAKE_STRENGTH(medium, 0, 1, 0)
        MAKE_STRENGTH(weak, 0, 0, 1)
    }
}

#endif
