(include "sqlite-ffi.scm")

;; (include "libfsl.scm")

(include "fossil-ffi.scm")

(define %%fossils%at-phone-decoder (make-parameter (lambda (x) #f)))

(cond-expand
 (debug
  (define-macro (assume obj msg . more)
    (let ((tmp (gensym 'assume)))
      `(let ((,tmp ,obj)) (if ,tmp ,tmp (apply error ,msg ,@more))))))
 (else
  (define-macro (assume obj msg . more) obj)))

(include "capture-domain.scm")

(define-macro (TBD-fork-failed-exit) '(exit 1))

(cond-expand
 ((or android fossil-not-embedded)
  (register-command!
   "fossil"
   (lambda (args)
     #|
     (exit
     (debug 'fossil-exit (process-status
     (open-process
     `(path: "fossil" arguments: ,args
     stdin-redirection: #f stdout-redirection: #f show-console: #f)))))
     |#
     (let ((conn (open-process
                  `(path: "fossil" arguments: ,args
                          stdin-redirection: #t stdout-redirection: #t show-console: #f))))
       (cond
        ((port? conn)
         (ports-connect! conn conn (current-input-port) (current-output-port))
         (exit (/ (process-status conn) 256)))
        (else (TBD-fork-failed-exit)))))))
 (else
  (include "../gamhack/gambit-embedded-chararray.scm")
  (c-declare "extern int fossil_main(int argc, char **argv);")
  ;; FIXME: Strangely this version closes the connection after 45-70 Kbyte.
  (register-command!
   "fossil"
   (lambda (args)
     (MATURITY -10 "fossil requires early dispatch" loc: "fossil")
     (let* ((args (cons "-s fossil" args))
            (n (length args)))
       ((c-lambda (int char**) int "fossil_main") n args))))))

(define $fossil-user-name ;; just distinguish from user-name
  (make-parameter (user-name)))

;;** Using Subprocess

(define (fossil-command
         #!key
         (log (and #f (lambda (args) (debug 'fossil-command args))))
         (directory (fossils-directory))
         (input #f)
         (repository #f)
         #!rest args)
  (let ((working-directory (or directory (current-directory)))
        (stderr-redirection #t)
        (arguments
         (cond
          ((not repository) args)
          ((string? repository) ;; TBD: file,exists,etc...
           (append args (list "-R" repository)))
          (else args))))
    (assume
     (begin
       (when (procedure? log)
         (log `(cwd: ,working-directory arguments: ,@arguments)))
       #t)
     "unreachable")
    (let ((port (semi-fork "fossil" arguments stderr-redirection directory: working-directory)))
      (cond
       ((not input) (close-output-port port))
       ((string? input)
        (display input port)
        (close-output-port port)))
      port)))

(define (fossil-command/option+rest
         #!key
         (log (and #f (lambda (args) (debug 'fossil-command args))))
         (directory (fossils-directory))
         (input #f)
         (repository #f)
         (command "version")
         (options '())
         (user ($fossil-user-name))
         #!rest args)
  (let ((working-directory (or directory (current-directory)))
        (stderr-redirection #t)
        (arguments
         `(,command
           ,@options
           ,@(if user `("-user" ,user) '())
           ,@(if repository `("-R" ,repository) '())
           ,@args)))
    (assume
     (begin
       (when (procedure? log)
         (log `(cwd: ,working-directory arguments: ,@arguments)))
       #t)
     "unreachable")
    (let ((port (semi-fork "fossil" arguments stderr-redirection directory: working-directory)))
      (cond
       ((not input) (close-output-port port))
       ((string? input)
        (display input port)
        (close-output-port port)))
      port)))

(define (fossil-command-port/json
         #!key
         (log (and #f (lambda (args) (debug 'fossil-command/json args))))
         (directory #f)
         (repository #t)
         (user ($fossil-user-name)))
  (let ((working-directory (or directory (current-directory)))
        (stderr-redirection #f)
        (arguments
         (let ((args `("json" "-json-input" "-" "-user" ,user)))
           (cond
            ((not repository) args)
            ((string? repository) ;; TBD: file,exists,etc...
             (append args (list "-R" repository)))
            (else args)))))
    (assume
     (begin
       (when (procedure? log)
         (log `(cwd: ,working-directory arguments: ,@arguments)))
       #t)
     "unreachable")
    (semi-fork "fossil" arguments stderr-redirection directory: working-directory)))

(define (fossil-command/json
         json-sexpr
         #!key
         (log (and #f (lambda (args) (debug 'fossil-command/json args))))
         (directory (fossils-directory))
         (repository #t)
         (user ($fossil-user-name)))
  (let ((port (fossil-command-port/json repository: repository user: user)))
    (json-write json-sexpr port)
    (close-output-port port)
    (let ((json (json-read port)))
      (cond
       ((eof-object? json) json)
       (else
        (let ((pl (assq 'payload json)))
          (cond
           (pl (cdr pl))
           (else
            (let ((msg (assq 'resultText json)))
              (cond
               ((pair? msg) (error (cdr msg)))
               (else (error "fossil-command/json" json))))))))))))

(define (fossil-project-title repository)
  (let ((json (fossil-command/json
               `((command . "config/get/project"))
               repository: repository)))
    (cond
     ((eof-object? json) #f)
     (else (cdr (assq 'project-name json))))))

(define (fossil-command/sql
         sql-string
         #!key
         (log (and #f (lambda (args) (debug 'fossil-command/sql args))))
         (directory (fossils-directory))
         (repository #f)
         (user ($fossil-user-name)))
  (let ((port
         (let ((working-directory (or directory (current-directory)))
               (stderr-redirection 'raise)
               (arguments
                (let ((args `("sql" "-user" ,user)))
                  (cond
                   ((not repository) args)
                   ((string? repository) ;; TBD: file,exists,etc...
                    (append args (list "-R" repository)))
                   (else args)))))
           (assume
            (begin
              (when (procedure? log)
                (log `(cwd: ,working-directory arguments: ,@arguments)))
              #t)
            "unreachable")
           (semi-fork "fossil" arguments stderr-redirection directory: working-directory))))
    (display sql-string port)
    (close-output-port port)
    port))

(define (sql-quote str)
  (define (string-index s char/char-set/pred i)
    (let loop ((i i))
      (and (< i (string-length s))
           (if (eqv? (string-ref s i) char/char-set/pred)
               i
               (loop (+ i 1))))))
  (if (string-index str #\' 0)
      (let loop ((off 0) (res '()))
        (let ((s (string-index str #\' off)))
          (if s
              (let ((e (string-index str #\' (+ s 1))))
                (loop (or e (string-length str))
                      `(,(case (string-ref str s)
                                   ((#\\) "\\\\")
                                   ((#\') "''")
                                   ;; ((#\")   "\\"" )
                                   ((#\x0) "\\0" )
                                   (else (error "internal error sql-quote broken")))
                                ,(substring str off s)
                                . ,res)))
              (apply
               string-append
               (reverse!      ; res is a fresh list
                (cons (substring str off (string-length str))
                      res))))))
      str))

;;** Using FFI

(define %fossil*sql-source%filename-history
  ;; I found this in libfossil/sql
  "
-- For a given file name, find all changes in the history of
-- that file, stopping at the point where it was added or
-- renamed.
SELECT
substr(b.uuid,0,12) as manifestUuid,
datetime(p.mtime) as manifestTime,
--       ml.*,
ml.mid AS manifestRid,
-- b.size AS manifestSize,
ml.pid AS parentManifestRid,
ml.fid AS fileContentRid

-- fileContentRid=0 at the point of a rename (under the old name). The
-- fields (manifest*, prevFileContentRid) will match at the rename
-- point across this query and the same query against the renamed
-- file. Only (fileContentRid, filename) always differ across the
-- rename-point records.
-- renamedEntry.fileContentRid=origEntry.prevFileContentRid if no
-- changes were made to the file between renaming and committing.
,
ml.pid AS prevFileContentRid
  -- prevFileContentRid=0 at start of history,
  -- prevFileContentRid=fileContentRid for a file which was just
  -- renamed UNLESS it was modified after the rename, in which case...
  -- ???
,
fn.name AS filename,
prevfn.name AS priorname
FROM
mlink ml, -- map of files/filenames to checkins
filename fn,
blob b, -- checkin manifest
plink p -- checkin heritage
-- This LEFT JOIN adds rename info:
LEFT JOIN filename prevfn ON ml.pfnid=prevfn.fnid
WHERE
ml.fnid=fn.fnid
AND ml.mid=b.rid
-- Interesting: with this is will ONLY report the rename point:
--   AND ml.pfnid=prevfn.fnid
AND p.cid=ml.mid -- renamed file will have non-0 here
ORDER BY manifestTime DESC
")

(define %fossil*sql-source%checkin
  "
SELECT blob.rid AS rid, tag.tagname AS sym, tagxref.mtime AS mtime
FROM leaf, blob, tag, tagxref
WHERE blob.rid=leaf.rid
-- AND tag.tagname='sym-'||'trunk'
AND tagxref.tagid=tag.tagid
AND tagxref.tagtype>1
AND tagxref.rid=leaf.rid
-- ORDER BY mtime DESC
-- LIMIT 1
")

(define (fossil-content/db+query+params db sql params)
  ;; TBD: use GGB for result and replace
  ;; `fossil-subu8vector-delta-apply` code parsing and applying the
  ;; delta to the current result.  Or maybe use GGB2D when appropriate.
  (define result #f)
  (define (row rid blob)
    (let ((blob (fossil-subu8vector-uncompress blob))
          (sofar result))
      (set! result (if sofar (fossil-subu8vector-delta-apply sofar blob) blob))
      #f))
  (sqlite3-exec* db sql params row: row)
  result)

(define (fossil-content/db+rid db rid)
  (define sql "
with Target(rid) AS (SELECT ?1),
linkage
as
(
 select blob.rid, srcid, 1 as level from blob left join delta on delta.rid=blob.rid
 join Target on blob.rid=Target.rid
 union all
 select delta.rid, delta.srcid, linkage.level+1 as level
 from linkage join delta on delta.rid=linkage.srcid
)
-- select * from linkage
select blob.rid, content from linkage join blob on linkage.rid=blob.rid
order by level desc
")
  (define result (fossil-content/db+query+params db sql (list rid)))
  (unless result (error "failed to load content from row with rid" fossil-content/db+rid db rid))
  result)

(define %fossil*sql-source%filename+brach->rid
  (string-append
   "WITH Target(filename,branch) AS (SELECT ?1, 'sym-'||?2),"
   "
Checkin(rid,sym,mtime) AS ("
   %fossil*sql-source%checkin
   "),
SelectedCheckin AS (
SELECT * FROM Checkin
WHERE sym IN (SELECT branch from Target)
ORDER BY mtime DESC
LIMIT 1
),
Ancestors AS (
 SELECT plink.*, 0 as level FROM SelectedCheckin JOIN plink ON cid=rid
 UNION ALL
 SELECT plink.*, Ancestors.level+1 as level FROM Ancestors JOIN plink ON Ancestors.pid=plink.cid
)
"
   "
SELECT
mlink.fid as rid
FROM Ancestors
JOIN mlink ON mlink.mid=Ancestors.cid
WHERE mlink.fnid=(
SELECT fnid from Target join filename on Target.filename=filename.name WHERE
filename.name=Target.filename
)
ORDER BY level
LIMIT 1
"
   ))

(define (%fossil-artifact->rid/db
         db artifact #!key
         (tag "trunk"))
  (define sql %fossil*sql-source%filename+brach->rid)
  (define result #f)
  (sqlite3-exec db sql row: (lambda (rid) (set! result rid)) artifact tag)
  result)

;;; Unfortunately this fails badly:
;;; (define (fossil-db-deltafunc-init! db)
;;;   (unless (eqv? ((c-lambda (sqlite3_db*) int "deltafunc_init") db) SQLITE_OK)
;;;     (error "failed to init fossil delta functions" db)))

(define (fossil-content repository artifact)
  (define (doit db)
    ;;; too badly it failes (fossil-db-deltafunc-init! db)
    (let ((rid
           (cond ;; TBD: sort out what the special case "string?" should be
            ((string? artifact) (%fossil-artifact->rid/db db artifact tag: "trunk"))
            (else (apply %fossil-artifact->rid/db db artifact)))))
      (and rid (fossil-content/db+rid db rid))))
  (call-with-sqlite3-database repository doit mode: 'r/o))

(define (open-fossil-content repository artifact)
  (let ((blob (fossil-content repository artifact)))
    (cond
     (blob (open-input-u8vector `(init: ,blob char-encoding: UTF-8)))
     (else (debug %fossil*sql-source%filename+brach->rid 'query) (error "artifact not found" open-fossil-content repository artifact)))))

;;*** Config

(define (fossil-config-set! repository key value)
  (define mtime
    (cond-expand
     (win32 (exact->inexact (current-seconds)))
     (else (current-seconds))))
  (sqlite3-file-command*!
   repository
   "insert or replace into config (name, value, mtime) values(?1, ?2, ?3)"
   (list key value mtime)))

(define (fossil-config-get-pin
         key #!key
         (repository
          ;; The used to use (beaver-local-unit-id) - until we needed it too fast.
          (fossils-project-filename (fossils-fallback-name (ot0-address))))
         (initial #f)
         (pred #f)
         (filter #f)
         (name key))
  ;; TBD: rewrite to collect all changes into a single call-with-sqlite3-database
  ;;
  ;; Usage: (kick (set! xx (fossil-config-get-pin "autosync")))
  (let ((result (make-pin initial: initial pred: pred filter: filter name: name)))
    (call-with-sqlite3-database
     repository
     (lambda (db)
       (let* ((sql "select value from config where name = ?1")
              (stmt (sqlite3-prepare db sql)))
         (sqlite3-bind! db stmt (list key))
         (MATURITY+2:sqlite3-for-each db result stmt)))
     mode: 'r/o)
    (wire!
     result critical:
     (lambda _
       (sqlite3-file-command*!
        repository "insert or replace into config(name, value) values(?1, ?2)"
        (list key (result)))))
    result))

(define (fossil-project-title-set! repository title)
  (fossil-config-set! repository "project-name" title))

(define (fossil-take-private! repository)
  ;; Mirrors (stolen from) fossils security_audit.c
  (call-with-sqlite3-database
   repository
   (lambda (db)
     (define tmstmp
       (cond-expand
        (win32 (exact->inexact (current-seconds)))
        (else (current-seconds))))
     (sqlite3-exec* db "UPDATE user SET cap='' WHERE login IN ('nobody','anonymous')" '())
     (sqlite3-exec* db "DELETE FROM config WHERE name='public-pages'" '())
     (sqlite3-exec*
      db "INSERT OR REPLACE INTO config (name, value, mtime) VALUES(?1, ?2, ?3)"
      (list "self-register" 0 tmstmp)))))

(define (fossil-user-create! repository login pw #!key (cap "") (info "") (photo (sql-null)))
  (define (doit db)
    (sqlite3-exec*
     db
     "insert or replace into user (login, pw, cap, cookie, ipaddr, cexpire, info, photo, mtime)
values(?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9)"
     (let ((cookie (sql-null))
           (ipaddr (sql-null))
           (cexpire (sql-null))
           (mtime
            (cond-expand
             (win32 (exact->inexact (current-seconds)))
             (else (current-seconds)))))
       (list login pw cap cookie ipaddr cexpire info  photo mtime)) ))
  (cond
   ((sqlite3-db? repository) (doit repository))
   (else (call-with-sqlite3-database repository doit))))

;;** fossils directory and service

(define (fossils-fallback-name unit-id)
  (beaver-unit-id->string unit-id "-"))

(define fossils-directory
  (make-pin
   initial: #f
   pred:
   (lambda (v)
     (or (not v)
         (and (string? v)
              (or (not (file-exists? v))
                  (eq? (file-type v) 'directory)))))
;;   filter: (lambda (old new) (if old old new)) ;; once only
   name: "projects directory"))

(define fossils-enable-http-hijacking
  ;; NOTE: This is experimental.  Switching this on may expose the
  ;; browser to cross site scripting attacks.
  (make-pin
   initial: #f
   name: "fossils-enable-http-hijacking dangerous feature"))

(define (fossils-directory-location dir)
  (cond-expand
   (android
    (make-pathname (system-directory) dir))
   (else dir)))

(define (fossils-project-filename project)
  (let ((dir (fossils-directory)))
    (and dir (make-pathname dir project "fossil"))))

(define MATURITY-1:beaver-default-unit-wiki-content (make-parameter #f))

(wire!
 (list fossils-directory beaver-local-unit-id)
 sequence:
 (lambda (oldd newd oa unit-id)
   (define (set-default-homepage-content! fn user name content)
     (fossil-command/json
      `((command . "wiki/create")
        (payload
         (name . ,name)
         (content . ,content)
         (mimetype . "text/x-markdown")
         (contentFormat . "raw")))
      repository: fn
      user: user))
   (when newd
     (unless (file-exists? newd)
       (create-directory newd))
     (when (and newd unit-id)
       (let* ((admin (fossils-fallback-name unit-id))
              (fn (fossils-project-filename admin)))
         (unless (file-exists? fn)
           (let* ((port (fossil-command directory: (current-directory) "init" "-A" admin fn))
                  (rc (process-status port)))
             ;;(fossil-command repository: fn "user" "new" "u") ;; FIXME: remove!
             (case rc
               ((0)
                (fossil-project-title-set! fn admin)
                (let loop ((content (MATURITY-1:beaver-default-unit-wiki-content)))
                  (cond
                   ((not content))
                   ((procedure? content) (loop (content unit-id)))
                   ((string? content) (set-default-homepage-content! fn admin admin content))
                   (else (error "fossils: default home page content: unhandled" content)))))
               (else (log-error "fossil init failed" port rc))))))))))

(define fossils-directory-handler
  (let ((v #f))
    (case-lambda
     (() v)
     ((n) (set! v n)))))

(define (fossils-copy-http-headers-catching-host in)
  (define host #f)
  (define (gather out)
    (let loop ()
      (let ((line (u8-read-line2 in 10 1024)))
        (cond
         ((or (equal? line "") (equal? line "\r"))
          (display "\r\n" out))
         (else
          (when (string-prefix? "Host: " line)
            (let* ((cr (string-ref line (fx- (string-length line) 1)))
                   (len (if (eqv? cr #\return) (- (string-length line) 1) (string-length line))))
              (set! host (substring line 6 len))))
          (display line out) (newline out) (loop))))))
  (let ((headers (call-with-output-string gather)))
    (values host headers)))

(define-macro (fossils-http-service-default/value name)
  (let ((option (string-append "-" (symbol->string name))))
    `(if ,name (list ,option ,name) '())))

(define-macro (fossils-http-service-default name)
  (let ((option (string-append "-" (symbol->string name))))
    `(if ,name (list ,option) '())))

(define (fossils-make-http-command-line-options
         repository #!key
         (notfound #f)
         (repolist #f)
         (https #f)
         (nossl #t)
         (nocompress #f)
         (localauth #f)
         (baseurl #f)
         (host #f)
         (ipaddr #f)
         (skin #f))
  (let ((notfound (fossils-http-service-default/value notfound))
        (repolist (fossils-http-service-default repolist))
        (https (fossils-http-service-default https))
        (nossl (fossils-http-service-default nossl))
        (nocompress (fossils-http-service-default nocompress))
        (localauth (fossils-http-service-default localauth))
        (baseurl (fossils-http-service-default/value baseurl))
        (host (fossils-http-service-default/value host))
        (ipaddr (fossils-http-service-default/value ipaddr))
        (skin (fossils-http-service-default/value skin))
        )
    `("http" ,repository
      ,@notfound
      ,@repolist
      ,@https
      ,@nossl
      ,@localauth
      ,@baseurl
      ,@host
      ,@ipaddr
      ,@skin
      ,@nocompress
      )))

(define fossils-http-serve
  (let ((brk (delay (rx "^([^ ]+) (?:/([^/]+))([^ ]+) (HTTP/[0-9]\\.[0-9])\r?$")))
        (max-line-length 1024))
    (define (fossil args)
      (semi-fork "fossil" args #t))
    (define (producer->pipe producer)
      (receive (servant client) (open-u8vector-pipe)
        (parameterize ((current-output-port servant))
          (producer)
          (close-port servant))
        client))
    (define (fossils-http-serve*
             local repository line0 scheme)
      (let* ((line (or line0
                       (u8-read-line2 (current-input-port) 10 max-line-length)))
             (m (rx~ (force brk) line))
             (unit-id (beaver-local-unit-id))
             (m2 (and m (uri-parse (rxm-ref m 2)))))
        (cond
         ((and m2 (equal? ((%%fossils%at-phone-decoder) m2) unit-id))
          (receive (host headers) (fossils-copy-http-headers-catching-host (current-input-port))
            (let* ((baseurl (string-append scheme  host "/" (rxm-ref m 2)))
                   (notfound (fossils-fallback-name unit-id))
                   (cmdln (if local
                              (fossils-make-http-command-line-options
                               repository
                               notfound: notfound
                               baseurl: baseurl
                               repolist: #t nocompress: #t
                               ipaddr: "127.0.0.1" localauth: #t)
                              (fossils-make-http-command-line-options
                               repository
                               notfound: notfound
                               baseurl: baseurl)))
                   (proc (fossil cmdln)))
              (when (port? proc)
                #;(let ((p (current-error-port)))
                (display line p) (newline p) (display headers p) (display "---\n" p))
                (display
                 (string-append (rxm-ref m 1) " " (rxm-ref m 3) " " (rxm-ref m 4) "\r\n")
                 proc)
                (display headers proc)
                (force-output proc)
                proc))))
         ((equal? line "")
          (producer->pipe
           (lambda ()
             (display
           #<<EOF
HTTP/1.0 500 Error
Content-Type: text/plain

fossils server could not read HTTP request.

EOF
           ))))
         (else
          (let* ((cmdln (if local
                            (fossils-make-http-command-line-options
                             repository
                             notfound: (fossils-fallback-name unit-id)
                             repolist: #t nocompress: #t
                             ipaddr: "127.0.0.1" localauth: #t)
                            (fossils-make-http-command-line-options
                             repository
                             notfound: (fossils-fallback-name unit-id))))
                 (proc (fossil cmdln)))
            (and (port? proc)
                 (begin
                   (display line proc) (newline proc)
                   (force-output proc)
                   proc)))))))
    (define (fossils-http-serve
             local repository line
             #!key
             (scheme "http://")
             (wait #t))
      (and repository
           (if (or line wait)
               (fossils-http-serve* local repository line scheme)
               (receive (port srv) (open-u8vector-pipe '(buffering: #t) '(buffering: #t))
                 (thread-start!
                  (make-thread
                   (lambda ()
                     (with-debug-exception-catcher
                      (lambda ()
                        (parameterize
                            ;; There should be a simpler way!
                            ((current-input-port srv) (current-output-port srv))
                          (let ((conn (fossils-http-serve* local repository #f scheme)))
                            (ports-connect! conn conn srv srv)
                            (process-status conn))))
                      `(fossils-http-serve ,repository)))
                   repository))
                 port))))
    fossils-http-serve))

(define fossils-directory-service
  (let ()
    (define (fossils-directory-service)
      (let ((dir (fossils-directory)))
        (when dir
          (let ((conn (fossils-http-serve #f dir #f)))
            (when (port? conn)
              (ports-connect! conn conn (current-input-port) (current-output-port)))))))
    (tag-thunk-as-service fossils-directory-service)))

(wire!
 (list fossils-directory beaver-local-unit-id)
 sequence:
 (let ((once #t)
       (brk #f)
       (max-line-length 1024))
   (lambda _
     (when (and once (fossils-directory) (beaver-local-unit-id))
       (set! once #f)
       (set! brk (rx "^([^ ]+) (?:/([^/]+))([^ ]+) (HTTP/[0-9]\\.[0-9])\r?$"))
       (let ((unit-id (beaver-local-unit-id))
             (previous-handler (http-proxy-on-illegal-proxy-request)))
         (fossils-directory-handler
          (lambda (auth) (fossils-http-serve auth (fossils-directory) #f wait: #f)))
         (http-proxy-on-illegal-proxy-request
          (lambda (line)
            (let* ((m (rx~ brk line))
                   (m2 (and m (uri-parse (rxm-ref m 2))))
                   (id (and m2 ((%%fossils%at-phone-decoder) m2))))
              (cond
               ((or (not id) (equal? id unit-id)
                    (file-exists? (fossils-project-filename (fossils-fallback-name id))))
                (let ((conn (fossils-http-serve #t (fossils-directory) line)))
                  (when (port? conn)
                    (ports-connect! conn conn (current-input-port) (current-output-port)))))
               ((fossils-enable-http-hijacking)
                (let ((conn (ot0cli-connect "local" id 80)))
                  (when (port? conn)
                    (display line conn) (newline conn)
                    (force-output conn)
                    (ports-connect! conn conn (current-input-port) (current-output-port)))))
               (else (previous-handler line)))))))))))
