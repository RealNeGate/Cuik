

(defnode TB_PTR_OFFSET
  ; identity rewrite
  :identity (lambda ((n Node)) (
    (let rhs (get-type (n 2)))

    ; ptr + 0 => ptr
    (if (is-con rhs 0) (n 1))
  ))

  ; bottom type tells the worst case lattice position
  :bottom (lambda (n) (lattice-allptr))
)

