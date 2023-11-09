(define (table-intersect table1 table2 f) (let
    ((sentinel (make-table))
     (acc (make-table)))
    (table-for-each
        (lambda (key1 val1)
            (let ((val2 (table-ref table2 key1 sentinel)))
                 (if (eq? val2 sentinel)
                    void
                    (table-set! acc key1 (f val1 val2)))))
        table1)
    acc))
