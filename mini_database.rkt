#lang racket
(define NULL 'null)

;====================================
;=            Cerința 1             =
;= Definirea elementelor de control =
;=          20 de puncte            =
;====================================

;= Funcțiile de acces
(define init-database
  (λ () (list)))

(define create-table
  (λ (table columns-name)
    (cons table (map (λ (x) (list x)) columns-name))))

(define get-name
  (λ (table)
    (car table)))

(define get-columns
  (λ (table)
    (map (λ (x) (car x)) (cdr table))))

(define get-tables
  (λ (db)
    db))

(define get-table
  (λ (db table-name)
    (car (foldl (λ (x acc) (if (equal? table-name (car x)) (cons x acc) acc )) (list) (get-tables db)))))

(define add-table
  (λ (db table)
    (append db (list table))))

(define remove-table
  (λ (db table-name)
    (foldl (λ (x acc) (if (equal? (car x) table-name) acc (cons x acc) )) (list) db)))


;= Pentru testare, va trebui să definiți o bază de date (având identificatorul db) cu următoarele tabele

;============================================================================================
;=                         Tabela Studenți                                                   =
;= +----------------+-----------+---------+-------+-------+                                  =
;= | Număr matricol |   Nume    | Prenume | Grupă | Medie |                                  =
;= +----------------+-----------+---------+-------+-------+                                  =
;= |            123 | Ionescu   | Gigel   | 321CA |  9.82 |                                  =
;= |            124 | Popescu   | Maria   | 321CB |  9.91 |                                  =
;= |            125 | Popa      | Ionel   | 321CC |  9.99 |                                  =
;= |            126 | Georgescu | Ioana   | 321CD |  9.87 |                                  =
;= +----------------+-----------+---------+-------+-------+                                  =
;=                                                                                           =
;=                                         Tabela Cursuri                                    =
;= +------+----------+-----------------------------------+---------------+------------+      =
;= | Anul | Semestru |            Disciplină             | Număr credite | Număr teme |      =
;= +------+----------+-----------------------------------+---------------+------------+      =
;= | I    | I        | Programarea calculatoarelor       |             5 |          2 |      =
;= | II   | II       | Paradigme de programare           |             6 |          3 |      =
;= | III  | I        | Algoritmi paraleli și distribuiți |             5 |          3 |      =
;= | IV   | I        | Inteligență artificială           |             6 |          3 |      =
;= | I    | II       | Structuri de date                 |             5 |          3 |      =
;= | III  | II       | Baze de date                      |             5 |          0 |      =
;= +------+----------+-----------------------------------+---------------+------------+      =
;============================================================================================
(define db (list (list "Studenți" (list "Număr matricol" 123 124 125 126)
                                 (list "Nume" "Ionescu" "Popescu" "Popa" "Georgescu")
                                 (list "Prenume" "Gigel" "Maria" "Ionel" "Ioana")
                                 (list "Grupă" "321CA" "321CB" "321CC" "321CD")
                                 (list "Medie" 9.82 9.91 9.99 9.87))
             (list "Cursuri" (list "Anul" "I" "II" "III" "IV" "I" "III")
                                (list "Semestru" "I" "II" "I" "I" "II" "II")
                                (list "Disciplină" "Programarea calculatoarelor" "Paradigme de programare" "Algoritmi paraleli și distribuiți" "Inteligență artificială" "Structuri de date" "Baze de date")
                                (list "Număr credite" 5 6 5 6 5 5)
                                (list "Număr teme" 2 3 3 3 3 0))
                             ))

;====================================
;=            Cerința 2             =
;=         Operația insert          =
;=            10 puncte             =
;====================================

(define check_atribut
  (λ (atribut record)
    (foldl (λ (intrare acc)
             (if (equal? (car intrare) (car atribut))
                 (append acc (cdr intrare))
                 acc
                 )) (list) record)))

(define modify_table
  (λ (tabel record)
    (foldl (λ (atribut acc_tab)
             (if (list? atribut)
                 (if (equal? (check_atribut atribut record) (list))
                     (append acc_tab (list (append atribut (list NULL))))
                     (append acc_tab (list (append atribut (list (check_atribut atribut record))))))
                 (append acc_tab (list atribut)))) (list) tabel)))

(define insert
  (λ (db table-name record)
    (foldl (λ (tabel acc_db)
             (if (equal? (car tabel) table-name)
                 (append acc_db (list (modify_table tabel record)))
                 (append acc_db (list tabel))
                 )) (list) db)))

;====================================
;=            Cerința 3 a)          =
;=     Operația simple-select       =
;=             10 puncte            =
;====================================
(define simple-select
  (λ (db table-name columns)
    (foldl (λ (coloana acc)
           (foldl (λ(col acc2)
                  (if (and (list? col) (equal? (car col) coloana)) (append acc2 (list (cdr col))) acc2 ) )
                acc (get-table db table-name)))
         (list) columns) ))

;====================================
;=            Cerința 3 b)          =
;=           Operația select        =
;=            30 de puncte          =
;====================================
(define minim
  (λ (l m)
    (if (null? l)
       m
       (if (equal? (min (car l) m) m)
           (minim (cdr l) m)
           (minim (cdr l) (car l))))))

(define maxim
  (λ (l m)
    (if (null? l)
       m
       (if (equal? (max (car l) m) m)
           (maxim (cdr l) m)
           (maxim (cdr l) (car l))))))

(define numara
  (λ (l)
    (length (remove-duplicates l))))

(define suma
  (λ (l)
    (apply + l)))

(define average
  (λ (l)
    (/ (suma l) (length l))))

(define sort_cresc
  (λ (l)
    (if (string? (car l))
        (sort l string<?)
        (sort l <))))

(define sort_desc
  (λ (l)
    (if (string? (car l))
        (sort l string>?)
        (sort l >))))

;====================================
;=              Streams             =
;====================================

(define (make-naturals k)
  (stream-cons k (make-naturals (add1 k))))

(define naturals-stream (make-naturals 0))

(define (stream-take s n)
  (cond ((zero? n) '())
        ((stream-empty? s) '())
        (else (cons (stream-first s)
                    (stream-take (stream-rest s) (- n 1))))))

;====================================

(define list_with_certain_indexes
  (λ (list indexes i)
    (if (null? list)
        '()
        (if (list? (member i indexes))
            (cons (car list) (list_with_certain_indexes (cdr list) indexes (+ 1 i)))
            (list_with_certain_indexes (cdr list) indexes (+ 1 i))))))

(define get_column_from_table
  (λ (column_name table)
    (foldl (λ (col acc)
             (if(equal? column_name (car col))
                (append acc (cdr col))
                 acc)) '() (cdr table))))

(define apply_condition
  (λ (col comp value)
    (indexes-where col (λ (record)
                       (comp record value)))))

(define find_indexes
  (λ (tabel conditions)
    (reverse (foldl (λ (x acc)
              (set-intersect acc (apply_condition (get_column_from_table (cadr x) tabel) (car x) (last x))))
           (stream-take naturals-stream (length (cadr tabel))) conditions))))

(define filter_col_by_conditions
  (λ (col table conditions)
    (list (list_with_certain_indexes
                                (get_column_from_table col table)
                                (find_indexes table conditions) 0))))

(define operatie
  (λ (lit list)
    (case lit
      ['min (minim list (car list))]
      ['max (maxim list (car list))]
      ['count (numara list)]
      ['sum (suma list)]
      ['avg (average list)]
      ['sort-asc (sort_cresc list)]
      ['sort-desc (sort_desc list)])))
            
(define select
  (λ (db table-name columns conditions)
    (foldl (λ(col acc)
             (if (string? col)
             (append acc (filter_col_by_conditions col (get-table db table-name) conditions))
             (append acc (list (operatie (car col) (car (filter_col_by_conditions (cdr col) (get-table db table-name) conditions)))))  )) '() columns)))


;====================================
;=             Cerința 4            =
;=           Operația update        =
;=            20 de puncte          =
;====================================

(define change-at-index
  (λ (list indexes value i)
    (if (null? list)
        '()
        (if (list? (member i indexes))
            (cons value (change-at-index (cdr list) indexes value (+ 1 i)))
            (cons (car list) (change-at-index (cdr list) indexes value (+ 1 i)))))))

(define need_for_update_col
  (λ (col values)
    (list? (member col (map car values)))))

(define update_table
  (λ (tabel values conditions)
    (foldl (λ (col acc)
             (if (list? col)
                 (if (need_for_update_col (car col) values)
                     (append acc (list (foldl (λ (pair acc2)
                                                (if (equal? (car pair) (car col))
                                                    (append acc2 (change-at-index col (find_indexes tabel conditions) (cdr pair) -1))
                                                    acc2)) '() values)))
                     (append acc (list col)))
                 (append acc (list col)))) '() tabel)))

(define update
  (λ (db table-name values conditions)
    (foldl (λ (tabel acc)
             (if (equal? table-name (car tabel))
                 (append acc (list (update_table tabel values conditions)))
                 (append acc (list tabel)))) '() db)))

;====================================
;=             Cerința 5            =
;=           Operația remove        =
;=              10 puncte           =
;====================================

(define list_with_the_other_indexes
  (λ (list indexes i)
    (if (null? list)
        '()
        (if (list? (member i indexes))
            (append '() (list_with_the_other_indexes (cdr list) indexes (+ 1 i)))
            (cons (car list) (list_with_the_other_indexes (cdr list) indexes (+ 1 i)))))))

(define remove_from_table
  (λ (tabel conditions)
    (foldl (λ (col acc)
             (if (list? col)
                 (if (null? conditions)
                     (append acc (list (list_with_the_other_indexes col (map sub1 (stream-take naturals-stream (length col))) -1)))
                     (append acc (list (list_with_the_other_indexes col (find_indexes tabel conditions) -1))))    
               (append acc (list col)))) '() tabel)))

(define delete
  (λ (db table-name conditions)
    (foldl (λ (tabel acc)
             (if (equal? table-name (car tabel))
                 (append acc (list (remove_from_table tabel conditions)))
                 (append acc (list tabel)))) '() db)))

;====================================
;=               Bonus              =
;=            Natural Join          =
;=            20 de puncte          =
;====================================
(define natural-join
  (λ (db tables columns conditions)
    '()))
