#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include <stdbool.h>

#include "jcommon.h"

// Storing global references statically for now; will obviously
// need to dynamically allocate space as programs grow.
static const int JNUM_GLOBAL_SLOTS = 20;
static struct JVal* jglobals[JNUM_GLOBAL_SLOTS] = { NULL };

// These counters keep track of heap allocations/frees.
extern int alive_heap_jval_counter;
extern int total_heap_jval_counter;
extern int alive_heap_int_counter;
extern int total_heap_int_counter;
extern int alive_heap_double_counter;
extern int total_heap_double_counter;
extern int alive_heap_jvalptrarray_counter;
extern int total_heap_jvalptrarray_counter;
extern int alive_heap_string_counter;
extern int total_heap_string_counter;

struct JVal* jval_heapalloc_int() {
    int* iptr;
    struct JVal* jval;

    // Allocate space for the JVal itself.
    jval = malloc(sizeof(struct JVal));
    if (!jval) {
        printf("ERROR: jval_heapalloc_int: call to malloc new JVal failed.\n");
        exit(EXIT_FAILURE);
    }
    alive_heap_jval_counter += 1;
    total_heap_jval_counter += 1;

    jval->type = JIntegerType;
    jval->loc = JLocHeapLocal;
    jval->rank = 0;
    jval->shape = NULL;

    iptr = malloc(sizeof(int));
    if (!iptr) {
        printf("ERROR: jval_heapalloc_int: call to malloc new int failed.\n");
        exit(EXIT_FAILURE);
    }
    alive_heap_int_counter += 1;
    total_heap_int_counter += 1;
    jval->ptr = iptr;
    return jval;
}

struct JVal* jval_heapalloc_double() {
    struct JVal* jval;
    double* dptr;

    // Allocate space for the JVal itself.
    jval = malloc(sizeof(struct JVal));
    if (!jval) {
        printf("ERROR: jval_heapalloc_double: call to malloc new JVal failed.\n");
        exit(EXIT_FAILURE);
    }
    alive_heap_jval_counter += 1;
    total_heap_jval_counter += 1;

    jval->type = JDoublePrecisionFloatType;
    jval->loc = JLocHeapLocal;
    jval->rank = 0;
    jval->shape = NULL;

    dptr = malloc(sizeof(double));
    if (!dptr) {
        printf("ERROR: jval_heapalloc_double: call to malloc new double failed.\n");
        exit(EXIT_FAILURE);
    }
    alive_heap_double_counter += 1;
    total_heap_double_counter += 1;
    jval->ptr = dptr;
    return jval;
}

struct JVal* jval_heapalloc_string(int length) {
    struct JVal* jval;
    char* sptr;

    // Allocate space for the JVal itself.
    jval = malloc(sizeof(struct JVal));
    if (!jval) {
        printf("ERROR: jval_heapalloc_string: call to malloc new JVal failed.\n");
        exit(EXIT_FAILURE);
    }
    alive_heap_jval_counter += 1;
    total_heap_jval_counter += 1;

    jval->type = JStringType;
    jval->loc = JLocHeapLocal;
    jval->rank = 1;  // strings are single dimension lists

    jval->shape = malloc(jval->rank * sizeof(int));
    if (!jval->shape) {
        printf("ERROR: jval_heapalloc_string: call to malloc new int for shape failed.\n");
        exit(EXIT_FAILURE);
    }
    alive_heap_int_counter += jval->rank;
    total_heap_int_counter += jval->rank;
    jval->shape[0] = length;

    sptr = malloc(length * sizeof(char));
    if (!sptr) {
        printf("ERROR: jval_heapalloc_string: call to malloc new string of length %d failed.\n", length);
        exit(EXIT_FAILURE);
    }
    alive_heap_string_counter += 1;
    total_heap_string_counter += 1;
    jval->ptr = sptr;
    return jval;
}

struct JVal* jval_heapalloc_array_dim1(int length) {
    struct JVal* jval;
    struct JVal** jvals;
    int* shape;

    // Allocate space for the JVal itself.
    jval = malloc(sizeof(struct JVal));
    if (!jval) {
        printf("ERROR: jval_heapalloc_array_dim1: call to malloc new JVal failed.\n");
        exit(EXIT_FAILURE);
    }
    alive_heap_jval_counter += 1;
    total_heap_jval_counter += 1;

    jval->type = JArrayType;
    jval->loc = JLocHeapLocal;
    jval->rank = 1;   // Hard-coded for now, this is a dimension 1 only function.

    jval->shape = malloc(jval->rank * sizeof(int));
    if (!jval->shape) {
        printf("ERROR: jval_heapalloc_array_dim1: call to malloc new int for shape failed.\n");
        exit(EXIT_FAILURE);
    }
    alive_heap_int_counter += jval->rank;
    total_heap_int_counter += jval->rank;
    jval->shape[0] = length;

    // Allocate space for *pointers* to the JVals that will comprise this array.
    jvals = malloc(length * sizeof(struct JVal*));
    if (!jvals) {
        printf("ERROR: jval_heapalloc_array_dim1: call to malloc %d new JVals*(s) failed.\n", length);
        exit(EXIT_FAILURE);
    }
    alive_heap_jvalptrarray_counter += 1;
    total_heap_jvalptrarray_counter += 1;
    jval->ptr = jvals;
    return jval;
}

struct JVal* jval_heapalloc_array_dim_n(struct JVal* shape_arr) {
    struct JVal* jval;
    struct JVal* reduce_intermediate;
    struct JVal** jvals;
    int length;

    if (shape_arr->type != JArrayType || shape_arr->rank != 1) {
        printf("ERROR: jval_heapalloc_array_dim_n: expected array of rank 1, got: (type:%d, rank:%d)",
            shape_arr->type, shape_arr->rank);
        exit(EXIT_FAILURE);
    }

    // Allocate space for the JVal itself.
    jval = malloc(sizeof(struct JVal));
    if (!jval) {
        printf("ERROR: jval_heapalloc_array_dim_n: call to malloc new JVal failed.\n");
        exit(EXIT_FAILURE);
    }
    alive_heap_jval_counter += 1;
    total_heap_jval_counter += 1;

    jval->type = JArrayNDimensionalType;
    jval->loc = JLocHeapLocal;
    // TODO: This will need to be changed over to use shape_fut
    jval->rank = shape_arr->shape[0];
    jval->shape_fut = jval_clone(shape_arr, JLocHeapLocal);

    reduce_intermediate = jreduce(JTimesOp, shape_arr);
    length = *(int*)reduce_intermediate->ptr;
    jval_drop(reduce_intermediate, false);

    // Allocate space for *pointers* to the JVals that will comprise this array.
    jvals = malloc(length * sizeof(struct JVal*));
    if (!jvals) {
        printf("ERROR: jval_heapalloc_array_dim_n: call to malloc %d new JVals*(s) failed.\n", length);
        exit(EXIT_FAILURE);
    }
    alive_heap_jvalptrarray_counter += 1;
    total_heap_jvalptrarray_counter += 1;
    jval->ptr = jvals;
    return jval;
}

struct JVal* jval_clone(struct JVal* jval, enum JValLocation loc) {
    struct JVal* ret;
    struct JVal** jvalsout;
    struct JVal** jvalsin;
    struct JVal* reduce_intermediate;
    int length;

    switch (jval->type) {
        case JIntegerType:
            ret = jval_heapalloc_int();
            ret->loc = loc;
            *(int*)ret->ptr = *(int*)jval->ptr;
            return ret;
        case JDoublePrecisionFloatType:
            ret = jval_heapalloc_double();
            ret->loc = loc;
            *(double*)ret->ptr = *(double*)jval->ptr;
            return ret;
        case JStringType:
            ret = jval_heapalloc_string(jval->shape[0]);
            ret->loc = loc;
            // Copy string's characters to this new string.
            for (int i = 0; i < jval->shape[0]; i++) {
                ((char*)ret->ptr)[i] = ((char*)jval->ptr)[i];
            }
            return ret;
        case JArrayType:
            ret = jval_heapalloc_array_dim1(jval->shape[0]);
            ret->loc = loc;
            jvalsin = (struct JVal**) jval->ptr;
            jvalsout = (struct JVal**) ret->ptr;
            for (int i = 0; i < jval->shape[0]; i++) {
                jvalsout[i] = jval_clone(jvalsin[i], loc);
            }
            return ret;
        case JArrayNDimensionalType:
            ret = jval_heapalloc_array_dim_n(jval->shape_fut);
            ret->loc = loc;
            jvalsin = (struct JVal**) jval->ptr;
            jvalsout = (struct JVal**) ret->ptr;

            reduce_intermediate = jreduce(JTimesOp, ret->shape_fut);
            length = *(int*)reduce_intermediate->ptr;
            jval_drop(reduce_intermediate, false);

            for (int i = 0; i < length; i++) {
                jvalsout[i] = jval_clone(jvalsin[i], loc);
            }
            return ret;
        default:
            printf("ERROR: jval_clone: unsupported type: %d\n", jval->type);
            exit(EXIT_FAILURE);
    }
}

// When JVals go out of scope, use this function to free them
// (will only free JVals on the heap, not on the stack or in global scope).
void jval_drop(struct JVal* jval, bool do_drop_globals) {
    struct JVal* reduce_intermediate;
    int length;

    switch (jval->loc) {
        case JLocStack:
            // Don't try to free stack memory; this JVal will be dropped
            // when its parent top-level stmt stack frame is popped.
            return;
        case JLocHeapLocal:
            // Continue to free local heap allocation.
            break;
        case JLocHeapGlobal:
            // Only drop globals if we've been explicitly asked to
            // (i.e., by caller at end of a scope).
            if (do_drop_globals) { break; }
            else                 { return; }
            exit(EXIT_FAILURE);
        default:
            printf("ERROR: jval_drop: unsupported address location: %d\n", jval->loc);
            exit(EXIT_FAILURE);
    }

    struct JVal** jvals;

//    printf("DROPPING type %d\n", jval->type);
    // First free the pointed-to value.
    switch (jval->type) {
        case JIntegerType:
            free(jval->ptr);
            alive_heap_int_counter -= 1;
            break;
        case JDoublePrecisionFloatType:
            free(jval->ptr);
            alive_heap_double_counter -= 1;
            break;
        case JStringType:
            free(jval->ptr);
            alive_heap_string_counter -= 1;
            break;
        case JArrayNDimensionalType:
            jvals = (struct JVal**) jval->ptr;

            reduce_intermediate = jreduce(JTimesOp, jval->shape_fut);
            length = *(int*)reduce_intermediate->ptr;
            jval_drop(reduce_intermediate, false);

            for (int i = 0; i < length; i++) {
                jval_drop(jvals[i], do_drop_globals);
            }

            alive_heap_jvalptrarray_counter -= 1;

            // Must free the shape_fut array
            jval_drop(jval->shape_fut, do_drop_globals);

            break;
        case JArrayType:
            jvals = (struct JVal**) jval->ptr;
            for (int i = 0; i < jval->shape[0]; i++) {
                jval_drop(jvals[i], do_drop_globals);
            }
            alive_heap_jvalptrarray_counter -= 1;
            break;
        default:
            printf("ERROR: jval_drop: unsupported type: %d", jval->type);
            exit(EXIT_FAILURE);
    }

    if (jval->type != JArrayNDimensionalType) {
        // Then free the internal shape array.
        free(jval->shape);
        alive_heap_int_counter -= jval->rank;  // Rank is # of dims, hence # of ints
    }

    // And finally free the JVal itself.
    free(jval);
    alive_heap_jval_counter -= 1;
}

void jglobal_set_reference(int global_id, struct JVal* jval) {

    // If another value is referred to, drop it first before shadowing it.
    // TODO: This will cause problems in the future if multiple variables
    // refer to the same value and that value is dropped.
    if (jglobals[global_id] != NULL) {
        jval_drop(jglobals[global_id], true);
    }

    // Update the reference.
    jglobals[global_id] = jval;
}

struct JVal* jglobal_get_reference(int global_id) {

    // ID must be within bounds of static global slot bank.
    if (global_id < 0 || global_id >= JNUM_GLOBAL_SLOTS) {
        printf("ERROR: get_global: illegal global_id: %d", global_id);
        exit(EXIT_FAILURE);
    }

    // Return the reference.
    return jglobals[global_id];
}

void jglobals_dropall() {

    for (int i = 0; i < JNUM_GLOBAL_SLOTS; i++) {
        if (jglobals[i] != NULL) {
            jval_drop(jglobals[i], true);
        }
    }
}

void jmemory_check(bool do_print_usage) {

    if (do_print_usage) {
        printf("=== MEMORY REPORT ================================================\n");
        printf("%d \\ %d\t\talive \\ historical JVals on heap\n",
            alive_heap_jval_counter, total_heap_jval_counter);
        printf("%d \\ %d\t\talive \\ historical ints on heap\n",
            alive_heap_int_counter, total_heap_int_counter);
        printf("%d \\ %d\t\talive \\ historical doubles on heap\n",
            alive_heap_double_counter, total_heap_double_counter);
        printf("%d \\ %d\t\talive \\ historical strings on heap\n",
            alive_heap_string_counter, total_heap_string_counter);
        printf("%d \\ %d\t\talive \\ historical JVal pointer arrays on heap\n",
            alive_heap_jvalptrarray_counter, total_heap_jvalptrarray_counter);
        printf("==================================================================\n");
    }

    // Require all heap allocations to have been freed in the course of execution.
    assert(alive_heap_jval_counter == 0);
    assert(alive_heap_int_counter == 0);
    assert(alive_heap_double_counter == 0);
    assert(alive_heap_string_counter == 0);
    assert(alive_heap_jvalptrarray_counter == 0);
}
