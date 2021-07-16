;; -*- Scheme -*-
;; SQLite FFI
;;
;; (C) 2021 JFW; License: BSD
;;
;; See tests/run.scm for documentation.

#;(declare
  (block)
  (not safe)
  ;; end of declarations
  )

(define sqlite3-debug-statements (make-parameter #f))

(c-declare #<<c-declare-end
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sqlite3.h>
#include <assert.h>

#define U8_DATA(obj) ___CAST(___U8*, ___BODY_AS (obj, ___tSUBTYPED))
#define U8_LEN(obj) ___HD_BYTES(___HEADER (obj))

#if 1

#define L_malloc(x) malloc(x)
#define L_free(x) free(x)

#else

int n_malloc = 0;

static void * L_malloc(size_t n)
{
 n_malloc++;
 fprintf(stderr, "malloced %d\n", n_malloc);
 return malloc(n);
}

static void L_free(void *x)
{
 n_malloc--;
 free(x);
}

#endif

static int rs_sqlite3_auth_unrestricted(void* userdata, int opcode,
				 const char* arg1, const char* arg2,
				 const char* dbname, const char* trigger)
{
  return SQLITE_OK;
}

static void sqlite3_set_authorizer_unrestricted(sqlite3 *cnx)
{
 sqlite3_set_authorizer(cnx, rs_sqlite3_auth_unrestricted, NULL);
}

/* ** AUTHORIZATION HANDLING ** */

static int rs_sqlite3_auth_restricted(void* userdata, int opcode,
			       const char* arg1, const char* arg2,
			       const char* dbname, const char* trigger)
{
  switch(opcode) {
  case SQLITE_CREATE_INDEX:	/* Index Name      Table Name      */
  case SQLITE_REINDEX:
  case SQLITE_CREATE_TABLE:	/* Table Name      NULL            */
  case SQLITE_CREATE_VTABLE:    /* Table Name      Module Name     */
  case SQLITE_ALTER_TABLE:      /* Database Name   Table Name      */
  case SQLITE_CREATE_TEMP_INDEX: /* Index Name      Table Name     */
  case SQLITE_CREATE_TEMP_TABLE: /* Table Name      NULL           */
  case SQLITE_CREATE_TEMP_TRIGGER: /* Trigger Name    Table Name   */
  case SQLITE_CREATE_TEMP_VIEW:	/* View Name       NULL            */
  case SQLITE_CREATE_TRIGGER:	/* Trigger Name    Table Name      */
  case SQLITE_CREATE_VIEW:	/* View Name       NULL            */
  case SQLITE_DELETE:		/* Table Name      NULL            */
  case SQLITE_DROP_INDEX:	/* Index Name      Table Name      */
  case SQLITE_DROP_TABLE:	/* Table Name      NULL            */
  case SQLITE_DROP_VTABLE:      /* Table Name      Module Name     */
  case SQLITE_DROP_TEMP_INDEX:	/* Index Name      Table Name      */
  case SQLITE_DROP_TEMP_TABLE:	/* Table Name      NULL            */
  case SQLITE_DROP_TEMP_TRIGGER: /* Trigger Name    Table Name     */
  case SQLITE_DROP_TEMP_VIEW:   /* View Name       NULL            */
  case SQLITE_DROP_TRIGGER:	/* Trigger Name    Table Name      */
  case SQLITE_DROP_VIEW:	/* View Name       NULL            */
  case SQLITE_INSERT:		/* Table Name      NULL            */
  case SQLITE_PRAGMA:		/* Pragma Name     1st arg or NULL */
  case SQLITE_READ:		/* Table Name      Column Name     */
  case SQLITE_SELECT:		/* NULL            NULL            */
#if SQLITE_VERSION_NUMBER > 3003007
  case SQLITE_FUNCTION:		/* Function Name   NULL            */
#endif
  case SQLITE_TRANSACTION:	/* NULL            NULL            */
  case SQLITE_UPDATE:		/* Table Name      Column Name     */
  case SQLITE_ANALYZE:          /* Table Name      NULL            */
  case SQLITE_RECURSIVE:        /* NULL      NULL                  */
    return SQLITE_OK;
  case SQLITE_ATTACH:		/* Filename        NULL            */
  case SQLITE_DETACH:		/* Database Name   NULL            */
  default:
fprintf(stderr, "auth_restricted deny %d\n", opcode);
    return SQLITE_DENY;
  }
}

/* KLUDGE: FIXME: we need to know what temp tables are, not code them hard */

static int is_temporary_table(void *userdata, const char *table)
{
 if (strcmp(table, "sqlite_temp_master") == 0) return 1;
 if (strcmp(table, "current_message") == 0) return 1;
 return 0;
}

static int rs_sqlite3_auth_restricted_ro(void* userdata, int opcode,
			          const char* arg1, const char* arg2,
				  const char* dbname, const char* trigger)
{
  switch(opcode) {
  case SQLITE_CREATE_INDEX:	/* Index Name      Table Name      */
  case SQLITE_CREATE_TABLE:	/* Table Name      NULL            */
  case SQLITE_ALTER_TABLE:      /* Database Name   Table Name      */
    return SQLITE_DENY;
  case SQLITE_CREATE_TEMP_INDEX: /* Index Name      Table Name     */
  case SQLITE_CREATE_TEMP_TABLE: /* Table Name      NULL           */
  case SQLITE_CREATE_TEMP_TRIGGER: /* Trigger Name    Table Name   */
  case SQLITE_CREATE_TEMP_VIEW:	/* View Name       NULL            */
    return SQLITE_OK;
  case SQLITE_CREATE_TRIGGER:	/* Trigger Name    Table Name      */
  case SQLITE_CREATE_VIEW:	/* View Name       NULL            */
  case SQLITE_DELETE:		/* Table Name      NULL            */
  case SQLITE_DROP_INDEX:	/* Index Name      Table Name      */
  case SQLITE_DROP_TABLE:	/* Table Name      NULL            */
    return SQLITE_DENY;
  case SQLITE_DROP_TEMP_INDEX:	/* Index Name      Table Name      */
  case SQLITE_DROP_TEMP_TABLE:	/* Table Name      NULL            */
  case SQLITE_DROP_TEMP_TRIGGER: /* Trigger Name    Table Name     */
  case SQLITE_DROP_TEMP_VIEW:   /* View Name       NULL            */
    return SQLITE_OK;
  case SQLITE_DROP_TRIGGER:	/* Trigger Name    Table Name      */
  case SQLITE_DROP_VIEW:	/* View Name       NULL            */
    return SQLITE_DENY;
  case SQLITE_INSERT:		/* Table Name      NULL            */
    if (is_temporary_table(userdata,arg1))
    return SQLITE_OK;
    return SQLITE_DENY;
  case SQLITE_PRAGMA:		/* Pragma Name     1st arg or NULL */
    return SQLITE_DENY;
  case SQLITE_READ:		/* Table Name      Column Name     */
  case SQLITE_SELECT:		/* NULL            NULL            */
#if SQLITE_VERSION_NUMBER > 3003007
  case SQLITE_FUNCTION:		/* Function Name   NULL            */
#endif
  case SQLITE_RECURSIVE:        /* NULL      NULL                  */
    return SQLITE_OK;
  case SQLITE_TRANSACTION:	/* NULL            NULL            */
    return SQLITE_DENY;
  case SQLITE_UPDATE:		/* Table Name      Column Name     */

  /* FIXME: this is somehow needed to select from fts tables. */
#if 0
    if (is_temporary_table(userdata,arg1))
    return SQLITE_OK;
    return SQLITE_DENY;
#else
    return SQLITE_OK;
#endif

  case SQLITE_ATTACH:		/* Filename        NULL            */
  case SQLITE_DETACH:		/* Database Name   NULL            */
  default:
    return SQLITE_DENY;
  }
}

static int sq_sqlite3_create_functions(sqlite3 *conn)
{
/*
  return sqlite3_create_function(conn, "concat", -1, SQLITE_UTF8 | SQLITE_DETERMINISTIC,
				 NULL, sqlite3_concat, NULL, NULL);
| */
}

static void sqlite3_set_authorizer_restricted_ro(sqlite3 *cnx)
{
 sqlite3_set_authorizer(cnx, rs_sqlite3_auth_restricted_ro, NULL);
}

static void sqlite3_set_authorizer_restricted(sqlite3 *cnx)
{
 sqlite3_set_authorizer(cnx, rs_sqlite3_auth_restricted, NULL);
}

/* setup function table */

static void sqlite3_setup_full(sqlite3 *cnx)
{
  sq_sqlite3_create_functions(cnx);
}

static void sqlite3_setup_restricted(sqlite3 *cnx)
{
  sqlite3_set_authorizer(cnx, rs_sqlite3_auth_restricted, NULL);
  sq_sqlite3_create_functions(cnx);
}

static void sqlite3_setup_restricted_ro(sqlite3 *cnx)
{
  sqlite3_set_authorizer(cnx, rs_sqlite3_auth_restricted_ro, NULL);
  sq_sqlite3_create_functions(cnx);
}

static void (*setup_table[4])(sqlite3 *) = {
  NULL,
  sqlite3_setup_full,
  sqlite3_setup_restricted,
  sqlite3_setup_restricted_ro
};

static void sqlite3_setup(sqlite3 *cnx, int i)
{
  void (*f)(sqlite3 *);
  assert(i<4);
  f=setup_table[i];
  if(f) (*f)(cnx);
}

/**** ommiting pthread stuff here for the time being ****/

typedef struct sqlite3_db {
  sqlite3 *cnx;
  size_t bufsize;
  void *buf;
} sqlite3_db;

static ___SCMOBJ gambit_free_sqlite_db (void *ptr)
{
 sqlite3_db* p=ptr;
 if(p!=NULL) {
   if(p->cnx) {
     sqlite3_close_v2(p->cnx);
     p->cnx=NULL;
   }
   if(p->buf) {
     free(p->buf);
     p->buf=NULL;
     p->bufsize=0;
   }
   free(p);
 }
 return ___FIX(___NO_ERR);
}


typedef struct prepare_args {
  sqlite3_stmt *stmt;
  int tail;
  sqlite3 *db;
  int sql_len;
  int offset;
  char sql[1];
} sqlite_prepare_args;

static ___SCMOBJ gambit_free_prepare_args (void *ptr)
{
 sqlite_prepare_args* p=ptr;
 if(p!=NULL) {
   free(p);
 }
 return ___FIX(___NO_ERR);
}

static ___SCMOBJ gambit_free_pointer (void *ptr)
{
 free(ptr);
 return ___FIX(___NO_ERR);
}

c-declare-end
)

(c-initialize #<<c-declare-end
if(sqlite3_initialize() != SQLITE_OK) {
  fprintf (stderr, "Warning: error initializing sqlite3 library\n");
}

c-declare-end
)

(define-macro (define-c-lambda id args ret ccode)
  `(define ,id (c-lambda ,args ,ret ,ccode)))

(define-macro (define-const symbol)
  `(define ,symbol
     ((c-lambda () int ,(string-append "___return (" (symbol->string symbol) ");")))))

(define-const SQLITE_OPEN_READONLY)
(define-const SQLITE_OPEN_READWRITE)
(define-const SQLITE_OPEN_CREATE)
(define-const SQLITE_OPEN_URI)
(define-const SQLITE_OPEN_MEMORY)
(define-const SQLITE_OPEN_NOMUTEX)
(define-const SQLITE_OPEN_FULLMUTEX)
(define-const SQLITE_OPEN_SHAREDCACHE)
(define-const SQLITE_OPEN_PRIVATECACHE)

(define-const SQLITE_OK)
(define-const SQLITE_ROW)
(define-const SQLITE_DONE)

(define-const SQLITE_INTEGER)
(define-const SQLITE_FLOAT)
(define-const SQLITE_BLOB)
(define-const SQLITE_NULL)
(define-const SQLITE_TEXT)

(c-declare #<<c-declare-end
static int ffi_sqlite3_prepare (sqlite_prepare_args*, sqlite3* db, const char *sql);
static int ffi_sqlite3_bind_blob (sqlite3_stmt* stmt, int col, ___SCMOBJ data);
static int ffi_sqlite3_bind_text (sqlite3_stmt* stmt, int col, const char *str);
static void ffi_sqlite3_column_blob (sqlite3_stmt* stmt, int col, ___SCMOBJ bytes);
static ___SCMOBJ gambit_free_pointer (void *ptr);
c-declare-end
)

;; (c-define-type sqlite3 "sqlite3")
;; (c-define-type sqlite3* (pointer sqlite3 (sqlite3*)))
(c-define-type sqlite3_stmt "sqlite3_stmt")
(c-define-type sqlite3_stmt* (pointer sqlite3_stmt (sqlite3_stmt*)))
(c-define-type sqlite3_db "sqlite3_db")
(c-define-type sqlite3_db* (pointer sqlite3_db (sqlite3_db*) "gambit_free_sqlite_db"))
(c-define-type sqlite_prepare_args "sqlite_prepare_args")
(c-define-type sqlite_prepare_args* (pointer sqlite_prepare_args (sqlite_prepare_args*) "gambit_free_prepare_args"))

(define (sqlite3-db? obj) (and (foreign? obj) (let ((f (foreign-tags obj))) (and f (eq? (car f) 'sqlite3_db*)))))

(define (sqlite3-stmt? obj) (and (foreign? obj) (let ((f (foreign-tags obj))) (and f (eq? (car f) 'sqlite3_stmt*)))))

(define sqlite3_errstr (c-lambda (int) UTF-8-string "___return ((char*)sqlite3_errstr (___arg1));"))

(define sqlite3-error-message
  (c-lambda (sqlite3_db*) UTF-8-string "___return((const char*)(___arg1 ? sqlite3_errmsg(___arg1->cnx) : \"connection lost\"));"))

(define-type sqlite3-error loc code db stmt more)

(define-macro (%%abort-sqlite3-error loc code db stmt . more)
  `(make-sqlite3-error ,loc (or (and ,db (sqlite3-error-message ,db)) ,code) ,db ,stmt (list ,@more)))

(define (sqlite3-open
         dbn #!optional
         (flags (bitwise-ior SQLITE_OPEN_READONLY SQLITE_OPEN_URI)))
  (define return (c-lambda (char-string int) sqlite3_db* "
 sqlite3* db;
 sqlite3_db* result;
 int rc = sqlite3_open_v2(___arg1, &db, ___arg2, NULL);
 if(rc != SQLITE_OK) {
  sqlite3_close_v2(db);
  ___return(NULL);
 }
 result=L_malloc(sizeof(sqlite3_db));
 result->cnx=db;
 result->buf=NULL;
 result->bufsize=0;
 ___return(result);
"))
  (return dbn flags))

(define (sqlite3-open/ro
         dbn #!optional
         (flags SQLITE_OPEN_URI))
  (sqlite3-open dbn (bitwise-ior SQLITE_OPEN_READONLY flags)))

(define (sqlite3-close db)
  (define sqlite3-close (c-lambda (sqlite3_db*) int "
   sqlite3_db* p=___arg1;
   sqlite3* dbc=p->cnx;
   sqlite3_stmt* stmt=NULL;
   int rc;
   if(dbc) {
     do {
       stmt=sqlite3_next_stmt(dbc, NULL);
       if(stmt!=NULL) {
         sqlite3_finalize(stmt);
       }
     } while(stmt!=NULL);
     rc=sqlite3_close(dbc);
     if(rc==SQLITE_OK) {
       p->cnx=NULL;
       if(p->buf) {
         free(p->buf);
         p->buf=NULL;
         p->bufsize=0;
       }
     }
   }
   ___return(rc);
"))
  (let ((rc (sqlite3-close db)))
    (if (eqv? rc SQLITE_OK) #t
	(raise (%%abort-sqlite3-error sqlite3-close rc db #f)))))

(define (sqlite3-prepare db sql)
  (define return
    (c-lambda
     (sqlite3_db* UTF-8-string size_t size_t) sqlite3_stmt*
     ;; db sql-string offset length
     "
 sqlite3_db* db=___arg1;
 sqlite3_stmt *stmt;
 int rc = sqlite3_prepare_v2(db->cnx, ___arg2+___arg3, ___arg4, &stmt, NULL);
 if(rc!=SQLITE_OK) {
   sqlite3_finalize(stmt);
   ___return(NULL);
 }
 ___return(stmt);
"))
  (return db sql 0 (string-length sql)))

(define sqlite3-column-count (c-lambda (sqlite3_stmt*) int "sqlite3_column_count"))

(define sqlite3-column-name (c-lambda (sqlite3_stmt* int) UTF-8-string "sqlite3_column_name"))

#| TBD ISSUE: documented in sqlite.org not found fossil
(define sqlite3_column_decltype (c-lambda (sqlite3_stmt* int) UTF-8-string "sqlite3_column_decltype"))
;;|#

(define-type sql-null)
(define sql-null make-sql-null)

(define (sqlite3-bind-index! db stmt i v)
  (cond
   ((u8vector? v)
    (let ((rc ((c-lambda
                (sqlite3_stmt* int scheme-object) int
                "___return(sqlite3_bind_blob(___arg1, ___arg2,
                           U8_DATA(___arg3), U8_LEN(___arg3),
                           /*SQLITE_STATIC*/ SQLITE_TRANSIENT));")
               stmt (+ i 1) v)))
      (if (eqv? rc SQLITE_OK) #f (%%abort-sqlite3-error 'bind! rc db stmt i v))))
   ((or (and (fixnum? v) v) (and (boolean? v) (if v 1 0)))
    => (lambda (v)
	 (let ((rc ((c-lambda (sqlite3_stmt* int int) int "sqlite3_bind_int")
		    stmt (+ i 1) v)))
	   (if (eqv? rc SQLITE_OK) #f (%%abort-sqlite3-error 'bind! rc db stmt i v)))))
   ((real? v)
    (let ((rc ((c-lambda (sqlite3_stmt* int double) int "sqlite3_bind_double")
	       stmt (+ i 1) v)))
      (if (eqv? rc SQLITE_OK) #f (%%abort-sqlite3-error 'bind! rc db stmt i v))))
   ((string? v)
    (let ((rc ((c-lambda (sqlite3_stmt* int UTF-8-string size_t) int
                         "___return(sqlite3_bind_text(___arg1, ___arg2, ___arg3, ___arg4, SQLITE_TRANSIENT));")
	       stmt (+ i 1) v (string-length v))))
      (if (eqv? rc SQLITE_OK) #f (%%abort-sqlite3-error 'bind! rc db stmt i v))))
   ((sql-null? v)
    (let ((rc ((c-lambda (sqlite3_stmt* int) int "sqlite3_bind_null")
	       stmt (+ i 1))))
      (if (eqv? rc SQLITE_OK) #f (%%abort-sqlite3-error 'bind! rc db stmt i v))))
   (else
    (%%abort-sqlite3-error "bind! blob, number, boolean, string or sql-null" #f db stmt i v))))

(define sqlite3_bind_int64 (c-lambda (sqlite3_stmt* int int64) int "sqlite3_bind_int64"))
(define sqlite3_bind_zeroblob (c-lambda (sqlite3_stmt* int int) int "sqlite3_bind_zeroblob"))
(define sqlite3_bind_parameter_count (c-lambda (sqlite3_stmt*) int "sqlite3_bind_parameter_count"))
(define sqlite3_clear_bindings (c-lambda (sqlite3_stmt*) int "sqlite3_clear_bindings"))

(define (sqlite3-bind! db stmt args)
  (let loop ((i 0) (args args))
    (if (null? args) #f
	(let ((rc (sqlite3-bind-index! db stmt i (car args))))
	  (if rc rc (loop (+ i 1) (cdr args)))))))

(define (sqlite3-column-blob stmt i)
  (let* ((n ((c-lambda (sqlite3_stmt* int) int "sqlite3_column_bytes") stmt i))
         (result (make-u8vector n)))
    (when (> n 0)
      ((c-lambda
        (sqlite3_stmt* int scheme-object size_t) void
        "memcpy(U8_DATA(___arg3), sqlite3_column_blob(___arg1, ___arg2), ___arg4);")
       stmt i result n))
    result))

(define sqlite3_data_count (c-lambda (sqlite3_stmt*) int "sqlite3_data_count"))

(define (sqlite3-columns st)
  (let* ((n (sqlite3-column-count st))
	 (result (##make-vector n)))
    (do ((i 0 (+ i 1)))
	((eq? i n) result)
      (##vector-set! result i (sqlite3-column-name st i)))))

(define-macro (sqlite3-run-fn root param fn cont)
  ;; source code compatibility no-op with Askemos code
  `(,cont (,fn ,param) ,param))

(define-macro (sqlite3-column-value statement i)
  (let ((type (gensym 'type)))
    `(let ((,type ((c-lambda (sqlite3_stmt* int) int "sqlite3_column_type") ,statement ,i)))
       (cond
        ((eq? ,type SQLITE_INTEGER)
         ((c-lambda (sqlite3_stmt* int) int64 "sqlite3_column_int64") ,statement ,i))
        ((eq? ,type SQLITE_FLOAT)
         ((c-lambda (sqlite3_stmt* int) double "sqlite3_column_double") ,statement ,i))
        ((eq? ,type SQLITE_NULL) (sql-null))
        ((eq? ,type SQLITE_TEXT)
         ((c-lambda (sqlite3_stmt* int) UTF-8-string "sqlite3_column_text") ,statement ,i))
        ((eq? ,type SQLITE_BLOB) (sqlite3-column-blob ,statement ,i))
        (else (error "Wrong sqlite3 column type"))))))

(define (sqlite3-values->vector st)
  (let* ((n (sqlite3-column-count st))
	 (result (make-vector n)))
    (do ((i 0 (+ i 1)))
	((eqv? i n) result)
      (vector-set! result i (sqlite3-column-value st i)))))

(define (call-with-sqlite3-values statement proc)
  (let* ((n (sqlite3-column-count statement)))
    (do ((i (- n 1) (- i 1))
         (result '() (cons (sqlite3-column-value statement i) result)))
	((eqv? i -1) (apply proc result)))))

(define #;-inline (sqlite3-for-each db stmt fn)
  (do ((exit #f))
      (exit #t)
    (sqlite3-run-fn
     (sqlite3-database-callback db) stmt (c-lambda (sqlite3_stmt*) int "sqlite3_step")
     (lambda (rc s)
       (cond
	((eqv? rc SQLITE_ROW) (fn s))
	((eqv? rc SQLITE_DONE) (set! exit #t) #f)
	(else
         (let ((exn (%%abort-sqlite3-error sqlite3-for-each rc db s '())))
           ((c-lambda (sqlite3_stmt*) int "sqlite3_finalize") stmt)
           (raise exn))))))))

(define (sqlite3-for-each* ;; opens and closes db
         dbn fn sql params #!key
         (mode #f)
         (flags SQLITE_OPEN_URI))
  (define (close-db db stmt)
    (let ((exn #f))
      (when (sqlite3-stmt? stmt)
        (unless (eqv? ((c-lambda (sqlite3_stmt*) int "sqlite3_finalize") stmt) SQLITE_OK)
          (set! exn (%%abort-sqlite3-error sqlite3-for-each* dbn db sql params))))
      (unless (sqlite3-close db) (raise (%%abort-sqlite3-error sqlite3-for-each* dbn db sql params)))
      (when exn (raise exn))))
  (cond ;; handle mode and flags composition, TBD reduce clutter
   ((not mode) (set! mode (bitwise-ior SQLITE_OPEN_READWRITE SQLITE_OPEN_CREATE flags)))
   ((integer? mode) (bitwise-ior mode flags))
   (else
    (case mode
     ((r/o) (set! mode (bitwise-ior SQLITE_OPEN_READONLY flags)))
     (else (set! mode (bitwise-ior SQLITE_OPEN_READWRITE SQLITE_OPEN_CREATE flags))))))
  (let ((db (sqlite3-open dbn mode)))
    (unless (sqlite3-db? db)
      (raise (%%abort-sqlite3-error sqlite3-for-each* #f #f sql params)))
    (let ((prepared
           (cond
            ((string? sql) (sqlite3-prepare db sql))
            (else
             (sqlite3-close db)
             (error "invalid SQL" dbn sql params)))))
      (cond
       ((sqlite3-stmt? prepared)
        (let ((exn (sqlite3-bind! db prepared params)))
          (when exn (close-db db prepared) (raise exn)))
        (with-exception-catcher
         (lambda (exn)
           (close-db db prepared)
           (raise exn))
         (lambda ()
           (sqlite3-for-each db prepared fn)
           (close-db db prepared))))
       (else
        (let ((exn (%%abort-sqlite3-error sqlite3-for-each* dbn db sql params)))
          (close-db db #f)
          (raise exn)))))))

(define (sqlite3-statement-finalize db stmt)
  (let ((v ((c-lambda (sqlite3_stmt*) int "sqlite3_finalize") stmt)))
    (or (eqv? v SQLITE_OK)
	(error (sqlite3-error-message db)))))

(define (sqlite3-statement-reset! db stmt args)
  (let ((rc ((c-lambda (sqlite3_stmt*) int "sqlite3_reset") stmt)))
    (if (eqv? rc SQLITE_OK) #t
	(raise (%%abort-sqlite3-error 'sqlite3:reset! rc db stmt args)))))

(define (sqlite3-exec/prepared db stmt args include-header)
  (if (sqlite3-debug-statements)
      (log-status (sqlite3-database-name db) ": \"" (sqlite3-statement-name stmt) "\" (prepared) on \n"  (object->string args)))
  (if (pair? args)
      (let ((exn (begin
		   (sqlite3-statement-reset! db stmt args)
		   (sqlite3-bind! db stmt args))))
	(if exn (begin (sqlite3-statement-reset! db stmt args) (raise exn)))))
  (let ((r '()))
    (sqlite3-for-each
     db stmt
     (lambda (stmt)
       (set! r (cons (sqlite3-values->vector stmt) r))))
    (let ((r0 (list->vector
	       (cond
                (include-header
                 (cons
		  (sqlite3-columns stmt)
		  (reverse! r)))
                (else (reverse! r))))))
      ;; See also "bind!": It is IMPORTANT that we keep a reference
      ;; to the args list here.
      ;;
      ;; BEWARE: if the compiler was to only preserve the boolean
      ;; value for (pair? args) we would loose badly…
      (if (pair? args) (sqlite3-statement-reset! db stmt args))
      r0)))

(define (sqlite3-exec->vectors db stmt . args) ;; deprecated
  (define include-header
    (cond
     ((eq? db header:)
      (let ((result stmt))
        (unless (pair? args) (error "to few arguments" sqlite3-exec))
        (set! db (car args))
        (set! args (cdr args))
        (unless (pair? args) (error "to few arguments" sqlite3-exec))
        (set! stmt (car args))
        (set! args (cdr args))
        result))
     (else #f)))
  (cond
   ((string? stmt)
    (let ((prepared (sqlite3-prepare db stmt)))
      (cond
       ((not prepared) (raise (%%abort-sqlite3-error sqlite3-exec #f db stmt)))
       (else
        (let ((result (sqlite3-exec/prepared db prepared args include-header)))
          (sqlite3-statement-finalize db prepared)
          result)))))
   (else (sqlite3-exec/prepared db stmt args include-header))))

(define (sqlite3-exec* db stmt args)
  (define (sqlite3-exec/prepared db stmt args)
    (if (sqlite3-debug-statements)
        (print port: (current-error-port) db ": \"" '(sqlite3-statement-name stmt) "\" (prepared) on \n"  (object->string args)))
    (if (pair? args)
        (let ((exn (begin
		     (sqlite3-statement-reset! db stmt args)
		     (sqlite3-bind! db stmt args))))
	  (if exn (begin (sqlite3-statement-reset! stmt args) (raise exn)))))
    (let* ((n (sqlite3-column-count stmt))
           (result (make-ggb size: n)))
      (sqlite3-for-each
       db stmt
       (lambda (stmt)
         (do ((i 0 (+ i 1)))
	     ((eqv? i n) result)
           (ggb-insert! result (sqlite3-column-value stmt i)))))
      ;; See also "bind!": It is IMPORTANT that we keep a reference
      ;; to the args list here.
      ;;
      ;; BEWARE: if the compiler was to only preserve the boolean
      ;; value for (pair? args) we would loose badly…
      (if (pair? args) (sqlite3-statement-reset! db stmt args))
      (cond
       ((eqv? n 0) #t)
       (else
        (let ((rows (/ (ggb-length result) n)))
          (make-mdvector
           (make-range (vector n rows))
           (ggb#ggb-buffer #;ggb->vector result)))))))
  (cond
   ((string? stmt)
    (let ((prepared (sqlite3-prepare db stmt)))
      (cond
       ((not prepared) (raise (%%abort-sqlite3-error sqlite3-exec #f db stmt)))
       (else
        (let ((result (sqlite3-exec/prepared db prepared args)))
          (sqlite3-statement-finalize db prepared)
          result)))))
   (else (sqlite3-exec/prepared db stmt args))))

(define (sqlite3-exec db stmt . args) (sqlite3-exec* db stmt args))

;;** File API

(define (call-with-sqlite3-database dbn proc #!key (mode #f))
  ;; draft
  (let ((db
         (case mode
           ((r r/o) (sqlite3-open/ro dbn))
           (else (sqlite3-open
                  dbn
                  (bitwise-ior SQLITE_OPEN_READWRITE SQLITE_OPEN_CREATE SQLITE_OPEN_URI))))))
    (cond
     (db
      (with-exception-catcher
       (lambda (exn)
         (if (eq? mode 'transaction) (sqlite3-exec db "rollback"))
         (sqlite3-close db)
         (raise exn))
       (lambda ()
         (case mode
           ((transaction) (sqlite3-exec db "begin DEFERRED transaction")))
         (let ((result (proc db)))
           (case mode
             ((transaction) (sqlite3-exec db "commit")))
           (sqlite3-close db)
           result))))
     (else (raise (%%abort-sqlite3-error with-sqlite3-database #f dbn 'open))))))

(define (sqlite3-file-query
         filename
         query #!key
         (params '())
         (accu (make-ggb))
         (row (lambda args (for-each (lambda (e) (ggb-insert! accu e)) args) accu))
         (out (lambda (e i) i)))
  (sqlite3-for-each*
   filename
   (lambda (step)
     (out (call-with-sqlite3-values step row) accu))
   query params
   mode: 'r/o)
  accu)

(define (sqlite3-file-command-for-each! dbn sql lst)
  ;; lst is a list of lists of SQL parameters
  (let ((db
         (sqlite3-open
          dbn
          (bitwise-ior SQLITE_OPEN_READWRITE SQLITE_OPEN_CREATE SQLITE_OPEN_URI)))
        (statement #f))
    (cond
     (db
      (with-exception-catcher
       (lambda (exn)
         (let ((msg
                (cond
                 ((sqlite3-error? exn) exn)
                 (else (sqlite3-error-message db)))))
           (when statement (sqlite3-statement-finalize db statement))
           (sqlite3-exec db "rollback")
           (sqlite3-close db)
           (if (sqlite3-error? exn) (raise exn) (error msg sql lst))))
       (lambda ()
         (sqlite3-exec db "begin DEFERRED transaction")
         (set! statement (sqlite3-prepare db sql))
         (let ((result
                (cond
                 ((not statement)
                  (error (sqlite3-error-message db)))
                 (else
                  (for-each
                   (lambda (params) (sqlite3-exec* db statement params))
                   lst)
                  (sqlite3-statement-finalize db statement)))))
           (sqlite3-exec db "commit")
           (sqlite3-close db)
           result))))
     (else (raise (%%abort-sqlite3-error with-sqlite3-database #f dbn 'open))))))

(define (sqlite3-file-command*! filename sql params)
  (define (return . _) (error "never reached for commands"))
  (sqlite3-for-each* filename return sql params))
