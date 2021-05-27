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
        (let ((result (sqlite3-exec db "select 1 as one")))
          (assume (equal? result ' #(#("one") #(1))) "unexpected result")
          (set! result (sqlite3-exec header: #f db "select 1 as one"))
          (assume (equal? result ' #(#(1))) "unexpected result without header")
          (set! result (sqlite3-exec header: #f db "select 'correct'"))
          (assume (equal? result '#(#("correct"))) "unexpected result for string")
          (sqlite3-close db))))
 ;;
 )

(test-assert
 "parameter binding"
 (let ((db (sqlite3-open "file:test.db?mode=memory")))
   (and (sqlite3-db? db)
        (let ((result (sqlite3-exec header: #f db "select ?1 as one" 1)))
          (assume (equal? result ' #(#(1))) "unexpected result for integer")
          (set! result (sqlite3-exec header: #f db "select ?1" 1.5))
          (assume (equal? result ' #(#(1.5))) "unexpected result for real number")
          (set! result (sqlite3-exec header: #f db "select ?1" #t))
          (assume (equal? result ' #(#(#t))) "unexpected result for #t")
          (set! result (sqlite3-exec header: #f db "select ?1" #f))
          (assume (equal? result ' #(#(#f))) "unexpected result for #f")
          (set! result (sqlite3-exec header: #f db "select ?1" (sql-null)))
          (assume (equal? result (vector (vector (sql-null)))) "unexpected result on NULL")
          (set! result (sqlite3-exec header: #f db "select ?1" '#u8(65)))
          (assume (equal? result ' #(#(#u8(65)))) "unexpected result for u8vector a.k.a. blob")
          (set! result (sqlite3-exec header: #f db "select ?1" "a"))
          (assume (equal? result ' #(#("a"))) "unexpected result for string")
          (sqlite3-close db))))
 ;;
 )
