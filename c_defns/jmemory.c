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

// All heap memory allocations must be done using this function.
struct JVal* jval_heapalloc(enum JValType type, int len) {
    struct JVal* jval;
    struct JVal** jvals;
    int* iptr;
    double* dptr;
    char* sptr;

    // Allocate space for the JVal itself.
    jval = malloc(sizeof(struct JVal));
    if (!jval) {
        printf("ERROR: jval_heapalloc: call to malloc new JVal failed.");
        exit(EXIT_FAILURE);
    }
    alive_heap_jval_counter += 1;
    total_heap_jval_counter += 1;

    jval->type = type;
    jval->loc = JLocHeapLocal;
    jval->len = len;

    // Allocate space for the type instance pointed to.
    switch (type) {
        case JIntegerType:
            iptr = malloc(len * sizeof(int));
            if (!iptr) {
                printf("ERROR: jval_heapalloc: call to malloc %d new int(s) failed.", len);
                exit(EXIT_FAILURE);
            }
            alive_heap_int_counter += 1;
            total_heap_int_counter += 1;
            jval->ptr = iptr;
            return jval;
        case JDoublePrecisionFloatType:
            dptr = malloc(len * sizeof(double));
            if (!dptr) {
                printf("ERROR: jval_heapalloc: call to malloc %d new double(s) failed.", len);
                exit(EXIT_FAILURE);
            }
            alive_heap_double_counter += 1;
            total_heap_double_counter += 1;
            jval->ptr = dptr;
            return jval;
        case JStringType:
            sptr = malloc(len * sizeof(char));
            if (!sptr) {
                printf("ERROR: jval_heapalloc: call to malloc new string of len %d failed.", len);
                exit(EXIT_FAILURE);
            }
            alive_heap_string_counter += 1;
            total_heap_string_counter += 1;
            jval->ptr = sptr;
            return jval;
        case JArrayType:
            // REMEMBER: This is just allocating space for _pointers_ to the JVals
            // that will eventually comprise the array, not the actual JVals themselves.
            jvals = malloc(len * sizeof(struct JVal*));
            if (!jvals) {
                printf("ERROR: jval_heapalloc: call to malloc %d new JVals*(s) failed.", len);
                exit(EXIT_FAILURE);
            }
            alive_heap_jvalptrarray_counter += 1;
            total_heap_jvalptrarray_counter += 1;
            jval->ptr = jvals;
            return jval;
        default:
            printf("ERROR: jval_heapalloc: unsupported type: %d", type);
            exit(EXIT_FAILURE);
    }
}

struct JVal* jval_clone(struct JVal* jval, enum JValLocation loc) {
    struct JVal* ret;
    struct JVal** jvalsout;
    struct JVal** jvalsin;
    ret = jval_heapalloc(jval->type, jval->len);
    ret->loc = loc;

    switch (jval->type) {
        case JIntegerType:
            *(int*)ret->ptr = *(int*)jval->ptr;
            return ret;
        case JDoublePrecisionFloatType:
            *(double*)ret->ptr = *(double*)jval->ptr;
            return ret;
        case JStringType:
            // Copy string's characters to this new string.
            for (int i = 0; i < jval->len; i++) {
                ((char*)ret->ptr)[i] = ((char*)jval->ptr)[i];
            }
            return ret;
        case JArrayType:
            jvalsin = (struct JVal**) jval->ptr;
            jvalsout = (struct JVal**) ret->ptr;
            for (int i = 0; i < jval->len; i++) {
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
        case JArrayType:
            jvals = (struct JVal**) jval->ptr;
            for (int i = 0; i < jval->len; i++) {
                jval_drop(jvals[i], do_drop_globals);
            }
            alive_heap_jvalptrarray_counter -= 1;
            break;
        default:
            printf("ERROR: jval_drop: unsupported type: %d", jval->type);
            exit(EXIT_FAILURE);
    }

    // Then free the JVal itself.
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
        printf("%d \\ %d\t\talive \\ historical JVal pointer arrays on heap\n",
            alive_heap_jvalptrarray_counter, total_heap_jvalptrarray_counter);
        printf("==================================================================\n");
    }

    // Require all heap allocations to have been freed in the course of execution.
    assert(alive_heap_jval_counter == 0);
    assert(alive_heap_int_counter == 0);
    assert(alive_heap_double_counter == 0);
    assert(alive_heap_jvalptrarray_counter == 0);
}
