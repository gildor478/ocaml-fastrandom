
/*
 * FastRandom generator, C parts 
 * author: Sylvain Le Gall
 *
 */

#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/bigarray.h>
#include <caml/fail.h>
#include <string.h>
#include <assert.h>

#define ST_SIZE 55
#define ST_SHIFT 24

typedef struct
{
  int st[ST_SIZE];
  int idx;
} fastrandom_t;

#define FastRandom_val(v) ((fastrandom_t *) Data_custom_val(v))

static int bits (fastrandom_t *rnd)
{
  int idx = (rnd->idx + 1) % ST_SIZE;
  int *st = rnd->st;
  int newval = (st[idx] + st[(idx + ST_SHIFT) % ST_SIZE]) & 0x3FFFFFFF;
  st[idx] = newval;
  rnd->idx = idx;
  return newval;
};

static struct custom_operations fastrandom_operations = {
  "com.ocamlcore.fastrandom",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

CAMLprim value caml_fastrandom_init (value vunit)
{
  CAMLparam1(vunit);
  register_custom_operations(&fastrandom_operations);
  CAMLreturn(Val_unit);
};

CAMLprim value caml_fastrandom_create (value varr)
{
  fastrandom_t *rnd = NULL;
  int i = 0;

  CAMLparam1(varr);
  CAMLlocal1(vres);

  if (Wosize_val(varr) != ST_SIZE)
  {
    caml_failwith("Not enough integer to initialize fastrandom structure");
  };

  vres = caml_alloc_custom(&fastrandom_operations, sizeof(fastrandom_t), 0, 1);
  rnd = FastRandom_val(vres);

  for (i = 0; i < ST_SIZE; i++)
  {
    rnd->st[i] = Int_val(Field(varr, i));
  };
  rnd->idx = 0;

  CAMLreturn(vres);
};

CAMLprim value caml_fastrandom_copy (value vrnd)
{
  fastrandom_t *rnd1 = NULL, *rnd2 = NULL;

  CAMLparam1(vrnd);
  CAMLlocal1(vres);

  vres = caml_alloc_custom(&fastrandom_operations, sizeof(fastrandom_t), 0, 1);

  rnd1 = FastRandom_val(vrnd);
  rnd2 = FastRandom_val(vres);

  rnd2->idx = rnd1->idx;
  memcpy(rnd2->st, rnd1->st, ST_SIZE * sizeof(int));

  CAMLreturn(vres);
};

CAMLprim value caml_fastrandom_bits (value vrnd)
{
  int res = 0;
  CAMLparam1(vrnd);
  res = bits(FastRandom_val(vrnd));
  CAMLreturn(Val_int(res));
};

CAMLprim value caml_fastrandom_refill (value vrnd, value vba)
{
  int           len = 0, i = 0;
  fastrandom_t *rnd;
  intnat       *data = NULL;

  CAMLparam2(vrnd, vba);

  assert(Bigarray_val(vba)->num_dims == 1);
  assert((Bigarray_val(vba)->flags & BIGARRAY_KIND_MASK) == BIGARRAY_CAML_INT);

  rnd = FastRandom_val(vrnd);
  data = Data_bigarray_val(vba);
  len = Bigarray_val(vba)->dim[0];

  for (i = 0; i < len; i++)
  {
    data[i] = bits(rnd);
  };

  CAMLreturn(Val_unit);
};

