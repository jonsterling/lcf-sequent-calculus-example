structure Pretty :
sig
  type prec = int
  type 'a pretty = prec -> 'a -> string
  val infix_ : prec -> 'a pretty * 'b pretty -> ('a * string * 'b) pretty
end = 
struct
  type prec = int
  type 'a pretty = prec -> 'a -> string

  fun paren b s = 
    if b then 
      "(" ^ s ^ ")"
    else
      s

  fun infix_ i (p1, p2) j (a, opr, b) = 
    paren (j > i) (p1 (i + 1) a ^ " " ^ opr ^ " " ^ p2 (i + 1) b)
end

structure Prop :> PROP = 
struct
  datatype prop = datatype SyntaxData.prop

  infixr 3 ~>
  infixr 4 /\ \/

  structure Prec = 
  struct
    val conj = 4
    val disj = 4
    val impl = 3
  end

  fun pretty k = 
    fn TRUE => "true"
     | FALSE => "false"
     | ` a => a
     | p /\ q => Pretty.infix_ Prec.conj (pretty, pretty) k (p, "/\\", q)
     | p \/ q => Pretty.infix_ Prec.disj (pretty, pretty) k (p, "\\/", q)
     | p ~> q => Pretty.infix_ Prec.impl (pretty, pretty) k (p, "~>", q)
end

structure Sequent : SEQUENT = 
struct
  type hyp = SyntaxData.hyp
  type context = SyntaxData.context
  datatype sequent = datatype SyntaxData.sequent

  infix ===>

  fun prettyCtx k = 
    fn [] => ""
     | [x] => Prop.pretty k x
     | x::xs => Prop.pretty k x ^ ", " ^ prettyCtx k xs

  fun pretty (hyps ===> p) = 
    Pretty.infix_ 0 (prettyCtx, Prop.pretty) 0 (hyps, "===>", p)
end