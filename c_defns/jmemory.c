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
extern int alive_heap_char_counter;
extern int total_heap_char_counter;
extern int alive_heap_jvalptrarray_counter;
extern int total_heap_jvalptrarray_counter;

int* heapalloc_ndim_col_widths_arr(int length) {
    int* arr;

    // Using clear-alloc to initialize all widths to zero.
    arr = calloc(length, sizeof(int));
    if (!arr) {
        printf("ERROR: heapalloc_ndim_col_widths_arr: call to malloc new int array failed.\n");
        exit(EXIT_FAILURE);
    }
    alive_heap_int_counter += length;
    total_heap_int_counter += length;

    return arr;
}

void heapfree_ndim_col_widths_arr(int* arr, int length) {
    free(arr);
    alive_heap_int_counter -= length;
    total_heap_int_counter -= length;
}

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

struct JVal* jval_heapalloc_char() {
    char* cptr;
    struct JVal* jval;

    // Allocate space for the JVal itself.
    jval = malloc(sizeof(struct JVal));
    if (!jval) {
        printf("ERROR: jval_heapalloc_char: call to malloc new JVal failed.\n");
        exit(EXIT_FAILURE);
    }
    alive_heap_jval_counter += 1;
    total_heap_jval_counter += 1;

    jval->type = JCharacterType;
    jval->loc = JLocHeapLocal;
    jval->rank = 0;
    jval->shape = NULL;

    cptr = malloc(sizeof(char));
    if (!cptr) {
        printf("ERROR: jval_heapalloc_char: call to malloc new char failed.\n");
        exit(EXIT_FAILURE);
    }
    alive_heap_char_counter += 1;
    total_heap_char_counter += 1;
    jval->ptr = cptr;
    return jval;
}

struct JVal* jval_heapalloc_array_dim_n_nonprimshape(int rank, struct JVal* shape_arr) {

    if (shape_arr->type != JArrayNDimensionalType || shape_arr->rank != 1) {
        printf("ERROR: jval_heapalloc_array_dim_n_nonprimshape: expected 1-dim array, got type:%d, rank:%d\n",
                shape_arr->type, shape_arr->rank);
        exit(EXIT_FAILURE);
    }

    int* ints = malloc(shape_arr->shape[0] * sizeof(int));
    if (!ints) {
        printf("ERROR: jval_heapalloc_array_dim_n_nonprimshape: call to malloc new ints failed.\n");
        exit(EXIT_FAILURE);
    }
    alive_heap_int_counter += shape_arr->shape[0];
    total_heap_int_counter += shape_arr->shape[0];

    struct JVal* elem;
    for (int i = 0; i < shape_arr->shape[0]; i++) {
        elem = ((struct JVal**)shape_arr->ptr)[i];
        if (elem->type != JIntegerType) {
            printf("ERROR: jval_heapalloc_array_dim_n_nonprimshape: expected integer, got type: %d\n", elem->type);
            exit(EXIT_FAILURE);
        }
        ints[i] = *(int*)((struct JVal**)shape_arr->ptr)[i]->ptr;
    }

    struct JVal* ndim_arr = jval_heapalloc_array_dim_n(rank, ints);
    // The primitive array was cloned by the callee, free our copy now.
    free(ints);
    alive_heap_int_counter -= shape_arr->shape[0];
    total_heap_int_counter -= shape_arr->shape[0];
    return ndim_arr;

}

struct JVal* jval_heapalloc_array_dim_n(int rank, int* shape) {
    struct JVal* jval;
    struct JVal* reduce_intermediate;
    struct JVal** jvals;
    int length;

    // Allocate space for the JVal itself.
    jval = malloc(sizeof(struct JVal));
    if (!jval) {
        printf("ERROR: jval_heapalloc_array_dim_n: call to malloc new JVal failed.\n");
        exit(EXIT_FAILURE);
    }
    alive_heap_jval_counter += 1;
    total_heap_jval_counter += 1;

    jval->type = JArrayNDimensionalType;
    jval->typaram = JNumeric;
    jval->loc = JLocHeapLocal;
    jval->rank = rank;

    // Make a clone of the shape array for attachment to this JVal.
    int* shape_clone = malloc(rank * sizeof(int));
    if (!shape_clone) {
        printf("ERROR: jval_heapalloc_array_dim_n: call to malloc for cloning shape failed.\n");
        exit(EXIT_FAILURE);
    }
    for (int i = 0; i < rank; i++) {
        shape_clone[i] = shape[i];
    }
    alive_heap_int_counter += rank;
    total_heap_int_counter += rank;
    jval->shape = shape_clone;

    length = jarray_length(jval);

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

int jarray_length(struct JVal* jval) {

    if (jval->type != JArrayNDimensionalType) {
        printf("ERROR: jarray_length: expected N-dimensional array, got: %d", jval->type);
        exit(EXIT_FAILURE);
    }

    int len = 1;
    for (int i = 0; i < jval->rank; i++) {
        len *= jval->shape[i];
    }
    return len;
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
        case JCharacterType:
            ret = jval_heapalloc_char();
            ret->loc = loc;
            *(char*)ret->ptr = *(char*)jval->ptr;
            return ret;
        case JArrayNDimensionalType:
            ret = jval_heapalloc_array_dim_n(jval->rank, jval->shape);
            ret->typaram = jval->typaram;
            ret->loc = loc;
            jvalsin = (struct JVal**) jval->ptr;
            jvalsout = (struct JVal**) ret->ptr;

            length = jarray_length(ret);
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
        case JCharacterType:
            free(jval->ptr);
            alive_heap_char_counter -= 1;
            break;
        case JArrayNDimensionalType:
            jvals = (struct JVal**) jval->ptr;
            length = jarray_length(jval);
            for (int i = 0; i < length; i++) {
                jval_drop(jvals[i], do_drop_globals);
            }
            alive_heap_jvalptrarray_counter -= 1;
            break;
        default:
            printf("ERROR: jval_drop: unsupported type: %d\n", jval->type);
            exit(EXIT_FAILURE);
    }

    // Then free the internal shape array.
    free(jval->shape);
    alive_heap_int_counter -= jval->rank;  // Rank is # of dims, hence # of ints

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
        printf("ERROR: get_global: illegal global_id: %d\n", global_id);
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
        printf("%d \\ %d\t\talive \\ historical chars on heap\n",
            alive_heap_char_counter, total_heap_char_counter);
        printf("%d \\ %d\t\talive \\ historical JVal pointer arrays on heap\n",
            alive_heap_jvalptrarray_counter, total_heap_jvalptrarray_counter);
        printf("==================================================================\n");
    }

    // Require all heap allocations to have been freed in the course of execution.
    assert(alive_heap_jval_counter == 0);
    assert(alive_heap_int_counter == 0);
    assert(alive_heap_double_counter == 0);
    assert(alive_heap_char_counter == 0);
    assert(alive_heap_jvalptrarray_counter == 0);
}
