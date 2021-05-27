;; (C) 2021 JFW

;; Run:
;;
;; /home/u/.cache/lambdanative/linux/calculator/calculator -s beaver -tests ../modules/fossils/tests/run.scm

(test-assert
 "sqlite3-open returns db and close works"
 (let ((db (sqlite3-open "file:test.db?mode=memory")))
   (and (sqlite3-db? db) (eq? (sqlite3-close db) #t))))

(test-assert
 "sqlite3-for-each #1"
 (let ((db (sqlite3-open "file:test.db?mode=memory")))
   (and (sqlite3-db? db)
        (let ((stmt (sqlite3-prepare db "select 1 as one")))
          (assume (sqlite3-stmt? stmt) "prepare failed" stmt)
          (sqlite3-for-each
           db stmt
           (lambda (stmt)
             (assume (equal? (sqlite3-columns stmt) '#("one")) "unexpected column name")
             (assume (eqv? (sqlite3-column-type stmt 0) SQLITE_INTEGER) "unexpected type")
             (assume (equal? (sqlite3-values->vector stmt) '#(1)) "unexpected result")))
          (sqlite3-finalize db stmt)
          (sqlite3-close db))))
 ;;
 )

(test-assert
 "sqlite3-for-each #2"
 (let ((db (sqlite3-open "file:test.db?mode=memory")))
   (and (sqlite3-db? db)
        (let ((stmt (sqlite3-prepare db "select 'one' as one")))
          (assume (sqlite3-stmt? stmt) "prepare failed" stmt)
          (sqlite3-for-each
           db stmt
           (lambda (stmt)
             (assume (equal? (sqlite3-columns stmt) '#("one")) "unexpected column name")
             (assume (eqv? (sqlite3-column-type stmt 0) SQLITE_TEXT) "unexpected type")
             (assume (equal? (sqlite3-values->vector stmt) '#("one")) "unexpected result")))
          (sqlite3-finalize db stmt)
          (sqlite3-close db))))
 ;;
 )

(test-assert
 "sqlite3-exec"
 (let ((db (sqlite3-open "file:test.db?mode=memory")))
   (and (sqlite3-db? db)
        (let ((result (sqlite3-exec->vectors db "select 1 as one")))
          (assume (equal? result '#(#("one") #(1))) "unexpected result")
          (set! result (sqlite3-exec db "select 1 as one"))
          (assume (equal? (mdvector-body result) '#(1)) "unexpected result without header")
          (set! result (sqlite3-exec db "select 'correct'"))
          (assume (equal? (mdvector-body result) '#("correct")) "unexpected result for string")
          (set! result (sqlite3-exec db "select 'correct', 23"))
          (assume (equal? (mdvector-body result) '#("correct" 23)) "unexpected result for string")
          (assume (eqv? (mdvector-ref result 0 1) 23) "wrong order")
          (sqlite3-close db))))
 ;;
 )

(test-assert
 "parameter binding"
 (let ((db (sqlite3-open "file:test.db?mode=memory")))
   (and (sqlite3-db? db)
        (let ((result (sqlite3-exec db "select ?1 as one" 1)))
          (define (check val msg)
            (assume (equal? (mdvector-ref result 0 0) val) msg))
          (check 1 "unexpected result for integer")
          (set! result (sqlite3-exec db "select ?1" 1.5))
          (check 1.5 "unexpected result for real number")
          (set! result (sqlite3-exec db "select ?1" #t))
          (check #t "unexpected result for #t")
          (set! result (sqlite3-exec db "select ?1" #f))
          (check #f "unexpected result for #f")
          (set! result (sqlite3-exec db "select ?1" (sql-null)))
          (assume (sql-null? (mdvector-ref result 0 0)) "unexpected result on NULL")
          (set! result (sqlite3-exec db "select ?1" '#u8(65)))
          (check '#u8(65) "unexpected result for u8vector a.k.a. blob")
          (set! result (sqlite3-exec db "select ?1" "a"))
          (check "a" "unexpected result for string")
          (sqlite3-close db))))
 ;;
 )

(test-assert
 "tables"
 (let ((db (sqlite3-open "file:test.db?mode=memory")))
   (and (sqlite3-db? db)
        (let ((result (sqlite3-exec db "create table p(x integer, y integer)"))
              (stmt (sqlite3-prepare db "insert into p values(?1, ?2)")))
          (sqlite3-exec db stmt 23 42)
          (sqlite3-exec db stmt 1 3)
          (sqlite3-finalize db stmt)
          (set! result (sqlite3-exec db "select * from p"))
          (assume
           (equal? (mdvector-body result) '#(23 42 1 3))
           "unexpected result table")
          (sqlite3-close db))))
 ;;
 )