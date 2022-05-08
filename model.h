#ifndef __model_h
#define	__model_h

#include <stdint.h>

typedef struct model_s model_t;

model_t* model_new(void);
void model_del(model_t*);

typedef struct c01_s {
	int32_t	c0, c1;
} c01_t;

c01_t model_current_bit_counts(model_t*);

void model_current_bit(model_t*, int); // integer is a boolean, actually - zero or non-zero.

void model_shift

#endif	/* __model_h */

