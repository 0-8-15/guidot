(c-declare
#<<end-of-c-declare

#include <chicken4/chicken.h>

#include <pthread.h>

static int the_initialized_flag = 0;

static void notify_about_exit()
{
  fprintf(stderr, "exit in kernel process %d\n", getpid());
}

static void exec_chicken_kernel(int argc, char **argv)
{
  C_word heap, stack, symbols;
  CHICKEN_parse_command_line(argc, argv, &heap, &stack, &symbols);
  CHICKEN_initialize(heap, stack, symbols, C_toplevel);
//  atexit(notify_about_exit);
  CHICKEN_run(C_toplevel);
  fprintf(stderr, "Unexpected termination in BALL (CHICKEN) toplevel.\n");
  exit(EXIT_FAILURE);
}

struct chicken_kernel_in_pthread_args {
  int argc;
  char **argv;
};

static void* chicken_kernel_in_pthread(void *arg)
{
  struct chicken_kernel_in_pthread_args *ap=arg;
  exec_chicken_kernel(ap->argc, ap->argv);
}

static int pthread_chicken_kernel(int argc, char **argv)
{
  static int kernel_running = 0;
  static pthread_t kt; // ignored until we do not exit(2) the process from kernel
  static struct chicken_kernel_in_pthread_args arg;
  int res = 0;
  if( kernel_running ) {
    fprintf(stderr, "Kernel supposed to run in pthread %d.\n", kt);
    return -1;
  }
  arg.argc = argc;
  arg.argv = argv;
  fprintf(stderr, "Starting kernel in pthread\n");
  res = pthread_create(&kt, NULL, chicken_kernel_in_pthread, &arg);
  if( res != 0 ) return res;
  kernel_running = 1;
  fprintf(stderr, "Created Kernel thread %d\n", kt);
  return pthread_detach(kt);
}

static int run_chicken_s2s(char *str, char *result, int size)
{
  C_word heap=0, stack=0, symbols=0;
  if( ! the_initialized_flag ) {
    // CHICKEN_parse_command_line(argc, argv, &heap, &stack, &symbols);
    the_initialized_flag = 1;
    CHICKEN_initialize(heap, stack, symbols, C_toplevel);
    CHICKEN_run(C_toplevel);
  }
  return CHICKEN_eval_string_to_string (str, result, size);
}

end-of-c-declare
)

;; Literally stolen from gambit manual:

(c-declare #<<c-declare-end

#include <stdlib.h>
#include <unistd.h>

extern char **environ;

char **get_environ (void) { return environ; }

void free_strings (char **strings)
{
  char **ptr = strings;
  while (*ptr != NULL)
    {
      ___EXT(___release_string) (*ptr);
      ptr++;
    }
  free (strings);
}

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

// ;;

#define ___BEGIN_CFUN_SCMOBJ_to_STRINGS_allocated(src,dst,i) \
if ((___err = SCMOBJ_to_STRINGS (___PSP src, &dst, i)) == ___FIX(___NO_ERR)) {
#define ___END_CFUN_SCMOBJ_to_STRINGS_allocated(src,dst,i) }

#define ___BEGIN_SFUN_SCMOBJ_to_STRINGS_allocated(src,dst) \
{ ___err = SCMOBJ_to_STRINGS (___PSP src, &dst, ___RETURN_POS);
#define ___END_SFUN_SCMOBJ_to_STRINGS(src,dst) }



c-declare-end
)


(c-define-type char** "char**" "STRINGS_to_SCMOBJ" "SCMOBJ_to_STRINGS" #t)

(c-define-type char**+ "char**" "STRINGS_to_SCMOBJ" "SCMOBJ_to_STRINGS_allocated" #t)

(define execv (c-lambda (char-string char**) int "execv"))
(define get-environ (c-lambda () char** "get_environ"))


(cond-expand
 ;; TBD: not for WIN32
 (else (c-declare #<<c-declare-end
#include <sys/types.h>
#include <sys/wait.h>
#include <errno.h>
typedef struct {
  int pid;
  int status;
  int norm;
  int sig;
} G_waitpid_arg;

c-declare-end
)
 (c-define-type G_waitpid (pointer "G_waitpid_arg"))
 (namespace ("chckn#" c-waitpid-arg c-waitpid))
 (define c-waitpid-arg (c-lambda () G_waitpid "___result_voidstar = malloc(sizeof(G_waitpid_arg));"))
 (define c-waitpid (c-lambda (G_waitpid int bool) void "
  G_waitpid_arg *a = ___arg1;
  if(a!=NULL) {
    a->pid = waitpid(___arg2, &a->status, ___arg3 ? WNOHANG : 0);
    if( a->pid==-1 ) {
      a->sig = errno;
      a->norm = 0;
    } else {
      a->norm = WIFEXITED(a->status);
      if(a->norm) a->sig = WEXITSTATUS(a->status);
      else if(WIFSIGNALED(a->status)) a->sig = WTERMSIG(a->status);
      else a->sig = WSTOPSIG(a->status);
    }
  }
  // ___result_voidstar = a;
"))

 (define (process-wait pid . nohang)
   (let loop ((status (c-waitpid-arg)))
     (c-waitpid status pid (and (pair? nohang) (car nohang)))
     (let ((r ((c-lambda (G_waitpid) int "___result = ___arg1->pid;") status))
           (norm ((c-lambda (G_waitpid) bool "___result = ___arg1->norm;") status))
           (sig ((c-lambda (G_waitpid) int "___result = ___arg1->sig;") status)))
       (if (= r -1)
           (if ((c-lambda (int) bool "___result = ___arg1 == EINTR;") sig)
               (loop status)
               (values sig #f -1)) ;;(error ((c-lambda (int) char-string "strerror") sig)
           (values sig norm r)))))

 (namespace (""))

))

(define (ballroll args)
  (set! args (cons "dummy" args))
  (let ((n (length args)))
    ((c-lambda (int char**) void "exec_chicken_kernel") n args)))

(define (ballroll-pthread args)
  (set! args (cons "dummy" args))
  (let* ((n (length args))
	 (res ((c-lambda (int char**+) int "pthread_chicken_kernel") n args)))
    (case res
      ((-1) (error "already running"))
      ((0) #t)
      (else (error "pthread create failed" res)))))

; (define cs2s (c-lambda (char-string char-string int) void "run_chicken_s2s"))

(define CHICKEN-eval-string-to-string
  (c-lambda (char-string char-string int) char-string "run_chicken_s2s"))
