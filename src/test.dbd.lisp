(ql:quickload :dbd-postgres)
(defvar *connection*
  (dbi:connect :postgres
               :database-name "kunabi"
               :username "kunabi"
               :password "kunabi"))

(let* ((query (dbi:prepare *connection*
                           "SELECT * FROM somewhere WHERE flag = ? OR updated_at > ?"))
       (result (dbi:execute query 0 "2011-11-01")))
  (loop for row = (dbi:fetch result)
     while row
     ;; process "row".
       ))
