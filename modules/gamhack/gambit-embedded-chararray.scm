;; Almost literally stolen from gambit manual.  Functions made static.

;; FIXME: check ___SCMOBJ_to_NONNULLSTRINGLIST that might do the same
;; trick better.

(c-declare #<<c-declare-end

#include <stdlib.h>
#include <unistd.h>

static void free_strings (char **strings)
{
  char **ptr = strings;
  while (*ptr != NULL)
    {
      ___EXT(___release_string) (*ptr);
      ptr++;
    }
  free (strings);
}

static
___SCMOBJ SCMOBJ_to_STRINGS (___PSD ___SCMOBJ src, char ***dst, int arg_num)
{
  /*
   * Src is a list of Scheme strings.  Dst will be a null terminated
   * array of C strings.
   */

  int i;
  ___SCMOBJ lst = src;
  int len = 4; /* start with a small result array */
  char **result = (char**) malloc (len * sizeof (char*));

  if (result == NULL)
    return ___FIX(___HEAP_OVERFLOW_ERR);

  i = 0;
  result[i] = NULL; /* always keep array null terminated */

  while (___PAIRP(lst))
    {
      ___SCMOBJ scm_str = ___CAR(lst);
      char *c_str;
      ___SCMOBJ ___err;

      if (i >= len-1) /* need to grow the result array? */
        {
          char **new_result;
          int j;

          len = len * 3 / 2;
          new_result = (char**) malloc (len * sizeof (char*));
          if (new_result == NULL)
            {
              free_strings (result);
              return ___FIX(___HEAP_OVERFLOW_ERR);
            }
          for (j=i; j>=0; j--)
            new_result[j] = result[j];
          free (result);
          result = new_result;
        }

      ___err = ___EXT(___SCMOBJ_to_CHARSTRING) (___PSP scm_str, &c_str, arg_num);

      if (___err != ___FIX(___NO_ERR))
        {
          free_strings (result);
          return ___err;
        }

      result[i++] = c_str;
      result[i] = NULL;
      lst = ___CDR(lst);
    }

  if (!___NULLP(lst))
    {
      free_strings (result);
      return ___FIX(___UNKNOWN_ERR);
    }

  /*
   * Note that the caller is responsible for calling free_strings
   * when it is done with the result.
   */

  *dst = result;
  return ___FIX(___NO_ERR);
}

static
___SCMOBJ STRINGS_to_SCMOBJ (___processor_state ___ps, char **src, ___SCMOBJ *dst, int arg_num)
{
  ___SCMOBJ ___err = ___FIX(___NO_ERR);
  ___SCMOBJ result = ___NUL; /* start with the empty list */
  int i = 0;

  while (src[i] != NULL)
    i++;

  /* build the list of strings starting at the tail */

  while (--i >= 0)
    {
      ___SCMOBJ scm_str;
      ___SCMOBJ new_result;

      /*
       * Invariant: result is either the empty list or a ___STILL pair
       * with reference count equal to 1.  This is important because
       * it is possible that ___CHARSTRING_to_SCMOBJ and ___make_pair
       * will invoke the garbage collector and we don't want the
       * reference in result to become invalid (which would be the
       * case if result was a ___MOVABLE pair or if it had a zero
       * reference count).
       */

      ___err = ___EXT(___CHARSTRING_to_SCMOBJ) (___ps, src[i], &scm_str, arg_num);

      if (___err != ___FIX(___NO_ERR))
        {
          ___EXT(___release_scmobj) (result); /* allow GC to reclaim result */
          return ___FIX(___UNKNOWN_ERR);
        }

      /*
       * Note that scm_str will be a ___STILL object with reference
       * count equal to 1, so there is no risk that it will be
       * reclaimed or moved if ___make_pair invokes the garbage
       * collector.
       */

      new_result = ___EXT(___make_pair) (___ps, scm_str, result);

      /*
       * We can zero the reference count of scm_str and result (if
       * not the empty list) because the pair now references these
       * objects and the pair is reachable (it can't be reclaimed
       * or moved by the garbage collector).
       */

      ___EXT(___release_scmobj) (scm_str);
      ___EXT(___release_scmobj) (result);

      result = new_result;

      if (___FIXNUMP(result))
        return result; /* allocation failed */
    }

  /*
   * Note that result is either the empty list or a ___STILL pair
   * with a reference count equal to 1.  There will be a call to
   * ___release_scmobj later on (in ___END_CFUN_STRINGS_to_SCMOBJ
   * or ___END_SFUN_STRINGS_to_SCMOBJ) that will allow the garbage
   * collector to reclaim the whole list of strings when the Scheme
   * world no longer references it.
   */

  *dst = result;
  return ___FIX(___NO_ERR);
}

#define ___BEGIN_CFUN_SCMOBJ_to_STRINGS(src,dst,i) \
if ((___err = SCMOBJ_to_STRINGS (___PSP src, &dst, i)) == ___FIX(___NO_ERR)) {
#define ___END_CFUN_SCMOBJ_to_STRINGS(src,dst,i) \
free_strings (dst); }

#define ___BEGIN_CFUN_STRINGS_to_SCMOBJ(src,dst) \
if ((___err = STRINGS_to_SCMOBJ (___ps, src, &dst, ___RETURN_POS)) == ___FIX(___NO_ERR)) {
#define ___END_CFUN_STRINGS_to_SCMOBJ(src,dst) \
___EXT(___release_scmobj) (dst); }

#define ___BEGIN_SFUN_STRINGS_to_SCMOBJ(src,dst,i) \
if ((___err = STRINGS_to_SCMOBJ (___ps, src, &dst, i)) == ___FIX(___NO_ERR)) {
#define ___END_SFUN_STRINGS_to_SCMOBJ(src,dst,i) \
___EXT(___release_scmobj) (dst); }

#define ___BEGIN_SFUN_SCMOBJ_to_STRINGS(src,dst) \
{ ___err = SCMOBJ_to_STRINGS (___PSP src, &dst, ___RETURN_POS);
#define ___END_SFUN_SCMOBJ_to_STRINGS(src,dst) }

c-declare-end
)

(c-define-type char** "char**" "STRINGS_to_SCMOBJ" "SCMOBJ_to_STRINGS" #t)

;; (define execv (c-lambda (char-string char**) int "execv"))
