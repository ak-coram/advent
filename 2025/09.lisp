(ql:quickload '(:cl-ppcre :duckdb))

(defun day09 (is-part-two)
  (ddb:with-transient-connection
    (ddb:run "INSTALL spatial" "LOAD spatial"
             "CREATE TABLE red_squares (i INTEGER PRIMARY KEY, pos POINT_2D)")
    (loop :for i :from 0 :for line :in (uiop:read-file-lines #P"./09.txt")
          :do (ppcre:register-groups-bind
                  ((#'parse-integer x y))
                  ("(\\d+),(\\d+)" line)
                (ddb:run `(,(ddb:concat "INSERT INTO red_squares (i, pos) "
                                        "VALUES (?, ST_Point2D(?, ?))")
                           (,i
                            ,(coerce x 'double-float)
                            ,(coerce y 'double-float)))))
          :finally
             (ddb:run (ddb:concat "CREATE VIEW closed_loop AS "
                                  "SELECT * FROM red_squares UNION ALL "
                                  "(SELECT " (write-to-string i) ", pos "
                                  " FROM red_squares WHERE i = 0)")))
    (ddb:run (ddb:concat
              "CREATE VIEW rectangles AS "
              "SELECT p.pos AS p, q.pos AS q, "
              "ST_Extent("
              "  ST_MakeLine([p.pos, q.pos]::GEOMETRY[])) AS rectangle, "
              "(1 + ABS(ST_X(p.pos)::INT64 - ST_X(q.pos)::INT64)) * "
              "  (1 + ABS(ST_Y(p.pos)::INT64 - ST_Y(q.pos)::INT64)) AS area "
              "FROM red_squares AS p JOIN red_squares AS q ON p != q "
              "ORDER BY area DESC"))
    (let* ((filter
             (when is-part-two
               '("WHERE ST_Within(rectangle, "
                 "  (SELECT ST_MakePolygon("
                 "     ST_MakeLine(ARRAY_AGG(pos ORDER BY i)::GEOMETRY[])) "
                 "   FROM closed_loop)) ")))
           (query (apply #'ddb:concat
                         `("SELECT area FROM rectangles " ,@filter "LIMIT 1"))))
      (ddb:get-result (ddb:q query) 'area 0))))

