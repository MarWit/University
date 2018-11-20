type 'a btree = Leaf | Node of 'a btree * 'a * 'a btree

let rec prod_c tree func =
    match tree with
        Leaf -> func 1
        Node (_, 0, _) -> 0
        Node (a, e, b) -> prod_c a ( prod_c b ( fun v -> e * v ) )
