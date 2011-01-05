#include "strings.h"
#include "object.h"
#include "runtime.h"
#include "arrays.h"
#include "modules.h"

#include "unicode/ustdio.h"
#include "unicode/unum.h"

#include <stdio.h>
#include <stdlib.h>

/* String Constants */

U_STRING_DECL(u_TRUE_s, "true", 4);
static bool u_TRUE_initted = false;
U_STRING_DECL(u_FALSE_s, "false", 5);
static bool u_FALSE_initted = false;

struct stringlist {
  struct stringlist *prev;
  int32_t len;
  UChar *s;
};

static void *vtable_java_lang_String[] = {
  method_java_Dlang_DString_Mclone_Rjava_Dlang_DObject,
  method_java_Dlang_DString_Mequals_Ajava_Dlang_DObject_Rscala_DBoolean,
  method_java_Dlang_DObject_Mfinalize_Rscala_DUnit,
  method_java_Dlang_DString_MhashCode_Rscala_DInt,
  method_java_Dlang_DString_MtoString_Rjava_Dlang_DString,
};

struct klass class_java_Dlang_DString = {
  { sizeof("java.lang.String") - 1, "java.lang.String" },
  sizeof(struct java_lang_String),
  &class_java_Dlang_DObject,
  vtable_java_lang_String,
  NULL,
  NULL,
  0,
};

UFILE* ustdout() {
  static UFILE *uout = NULL;
  if (uout == NULL) {
    uout = u_finit(stdout, NULL, NULL);
  }
  return uout;
}

void
method_java_Dlang_DString_M_Linit_G_Rjava_Dlang_DString(
    struct java_lang_String* s)
{
  method_java_Dlang_DObject_M_Linit_G_Rjava_Dlang_DObject((struct java_lang_Object*)s);
}

void
method__Oscala_DConsole_Mprint_Ajava_Dlang_DString_Rscala_DUnit(
    struct java_lang_Object *module,
    struct java_lang_String *s)
{
  u_file_write(s->s, s->len, ustdout());
}

struct java_lang_String*
rt_makestring(struct utf8str *s)
{
  enum UErrorCode uerr = U_ZERO_ERROR;
  struct java_lang_String *ret = (struct java_lang_String*)rt_new(&class_java_Dlang_DString);
  UChar *buffer;
  int32_t bufsize = s->len;
  int32_t reqsize;
  method_java_Dlang_DString_M_Linit_G_Rjava_Dlang_DString(ret);
  buffer = malloc(bufsize * sizeof(UChar));
  u_strFromUTF8(buffer, bufsize, &reqsize, s->bytes, s->len, &uerr);
  if (uerr == U_BUFFER_OVERFLOW_ERROR) {
    /* reallocate buffer and retry */
    free(buffer);
    buffer = malloc(reqsize * sizeof(UChar));
    bufsize = reqsize;
    uerr = U_ZERO_ERROR;
    u_strFromUTF8(buffer, bufsize, &reqsize, s->bytes, s->len, &uerr);
  }
  if (U_SUCCESS(uerr)) {
    ret->len = reqsize;
    ret->s = buffer;
    return ret;
  } else {
    return NULL;
  }
}

void rt_string_append_Boolean(
    struct stringlist **s,
    uint8_t v)
{
  struct stringlist *n = malloc(sizeof(struct stringlist));
  n->prev = *s;
  *s = n;
  if (v) {
    if (!u_TRUE_initted) {
      U_STRING_INIT(u_TRUE_s, "true", 4);
      u_TRUE_initted = true;
    }
    n->len = 4;
    n->s = u_TRUE_s;
  } else {
    if (!u_FALSE_initted) {
      U_STRING_INIT(u_FALSE_s, "false", 5);
      u_FALSE_initted = true;
    }
    n->len = 4;
    n->s = u_FALSE_s;
  }
}

UNumberFormat *ufmt() {
  static UNumberFormat res = NULL;
  if (res == NULL) {
    UErrorCode status = U_ZERO_ERROR;
    res = unum_open(UNUM_DEFAULT, NULL, -1, NULL, NULL, &status);
  }
  return res;
}

void rt_string_append_number(
    struct stringlist **s,
    int32_t v,
    int32_t initsize)
{
  struct stringlist *n = malloc(sizeof(struct stringlist));
  n->prev = *s;
  *s = n;
  {
    UErrorCode err = U_ZERO_ERROR;
    int32_t bufsize = initsize;
    int32_t reqsize;
    UChar *buffer = malloc(bufsize * sizeof(UChar));
    reqsize = unum_format(ufmt(), v, buffer, bufsize, NULL, &err);
    if (err == U_BUFFER_OVERFLOW_ERROR) {
      err = U_ZERO_ERROR;
      free(buffer);
      buffer = malloc(reqsize * sizeof(UChar));
      bufsize = reqsize;
      reqsize = unum_format(ufmt(), v, buffer, bufsize, NULL, &err);
    }
    if (U_SUCCESS(err)) {
      n->len = reqsize;
      n->s = buffer;
    } else {
      *s = n->prev;
    }
  }
}

void rt_string_append_Byte(
    struct stringlist **s,
    int8_t v)
{
  rt_string_append_number(s, v, 4);
}

void rt_string_append_Short(
    struct stringlist **s,
    int16_t v)
{
  rt_string_append_number(s, v, 6);
}

void rt_string_append_Char(
    struct stringlist **s,
    UChar32 v)
{
  int32_t i = 0;
  struct stringlist *n = malloc(sizeof(struct stringlist));
  n->prev = *s;
  *s = n;
  if (U_IS_BMP(v)) {
    n->s = malloc(sizeof(UChar));
    n->len = 1;
  } else {
    n->s = malloc(sizeof(UChar)*2);
    n->len = 2;
  }
  U16_APPEND_UNSAFE(n->s, i, v);
}

void rt_string_append_Int(
    struct stringlist **s,
    int32_t v)
{
  rt_string_append_number(s, v, 11);
}

void rt_string_append_Long(
    struct stringlist **s,
    int64_t v)
{
  struct stringlist *n = malloc(sizeof(struct stringlist));
  n->prev = *s;
  *s = n;
  {
    UErrorCode err = U_ZERO_ERROR;
    int32_t bufsize = 20;
    int32_t reqsize;
    UChar *buffer = malloc(bufsize * sizeof(UChar));
    reqsize = unum_formatInt64(ufmt(), v, buffer, bufsize, NULL, &err);
    if (err == U_BUFFER_OVERFLOW_ERROR) {
      err = U_ZERO_ERROR;
      free(buffer);
      buffer = malloc(reqsize * sizeof(UChar));
      bufsize = reqsize;
      reqsize = unum_formatInt64(ufmt(), v, buffer, bufsize, NULL, &err);
    }
    if (U_SUCCESS(err)) {
      n->len = reqsize;
      n->s = buffer;
    } else {
      *s = n->prev;
    }
  }
}

void rt_string_append_Double(
    struct stringlist **s,
    double v)
{
  struct stringlist *n = malloc(sizeof(struct stringlist));
  n->prev = *s;
  *s = n;
  {
    UErrorCode err = U_ZERO_ERROR;
    int32_t bufsize = 4;
    int32_t reqsize;
    UChar *buffer = malloc(bufsize * sizeof(UChar));
    reqsize = unum_formatInt64(ufmt(), v, buffer, bufsize, NULL, &err);
    if (err == U_BUFFER_OVERFLOW_ERROR) {
      err = U_ZERO_ERROR;
      free(buffer);
      buffer = malloc(reqsize * sizeof(UChar));
      bufsize = reqsize;
      reqsize = unum_format(ufmt(), v, buffer, bufsize, NULL, &err);
    }
    if (U_SUCCESS(err)) {
      n->len = reqsize;
      n->s = buffer;
    } else {
      *s = n->prev;
    }
  }
}

void rt_string_append_Float(
    struct stringlist **s,
    float v)
{
  rt_string_append_Double(s, v);
}

typedef struct java_lang_String* (*toStringFn)(struct java_lang_Object*);

void rt_string_append_string(
    struct stringlist **s,
    struct java_lang_Object *sobj)
{
  struct java_lang_String *ss;
  toStringFn toString;
  struct stringlist *n = malloc(sizeof(struct stringlist));
  n->prev = *s;
  *s = n;

  toString = sobj->klass->vtable[4];
  ss = toString(sobj);

  n->len = ss->len;
  n->s = ss->s;
}

int32_t method_java_Dlang_DString_MhashCode_Rscala_DInt(struct java_lang_String* self)
{
  int32_t hashcode = 0;
  for (int32_t i = 0; i < self->len; i++) {
    hashcode += self->s[i] * 13;
  }
  return hashcode;
}

bool method_java_Dlang_DString_Mequals_Ajava_Dlang_DObject_Rscala_DBoolean(struct java_lang_String* self, struct java_lang_Object* other)
{
  if (other->klass == &class_java_Dlang_DString) {
    struct java_lang_String *os = (struct java_lang_String*)other;
    return 0 == u_strCompare(self->s, self->len, os->s, os->len, TRUE);
  } else {
    return false;
  }
}

struct java_lang_Object *method_java_Dlang_DString_Mclone_Rjava_Dlang_DObject(struct java_lang_String* self)
{
  return (struct java_lang_Object*)self;
}

struct java_lang_String *method_java_Dlang_DString_MtoString_Rjava_Dlang_DString(struct java_lang_String* self)
{
  return self;
}

struct java_lang_String *
rt_stringconcat(struct stringlist **s)
{
  struct java_lang_String *ret = (struct java_lang_String*)rt_new(&class_java_Dlang_DString);
  method_java_Dlang_DString_M_Linit_G_Rjava_Dlang_DString(ret);
  UChar *buffer;
  UChar *wp;
  int32_t totlen = 0;
  struct stringlist *head = *s;
  while (head != NULL) {
    totlen += head->len;
    head = head->prev;
  }
  buffer = malloc(totlen * sizeof(UChar));
  wp = &buffer[totlen];
  head = *s;
  while (head != NULL) {
    //puts("x");
    u_memcpy(wp - head->len, head->s, head->len);
    wp -= head->len;
    head = head->prev;
  }
  ret->len = totlen;
  ret->s = buffer;
  return ret;
}