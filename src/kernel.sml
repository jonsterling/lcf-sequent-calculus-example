structure Kernel :> KERNEL =
struct
  structure P = Prop and S = Sequent

  open P S
  infixr 0 ===>
  infixr 3 ~>
  infixr 4 /\ \/

  datatype term = datatype TermData.term
  datatype proof = BY of sequent * proof term
  infix BY

  fun out (_ BY m) = m

  fun infer (ctx ===> p BY _) = ctx ===> p
  fun check (ctx ===> p) (ctx' ===> p' BY _) =
    if ctx = ctx' andalso p = p' then () else
      raise Fail "Checking failed"

  fun init (ctx, i) =
    ctx ===> List.nth (ctx, i) BY INIT i
    handle _ => raise Fail "init"

  fun trueR ctx =
    ctx ===> TRUE BY TRUER
  
  fun falseL (ctx, i, p) =
    let
      val FALSE = List.nth (ctx, i)
    in
      ctx ===> p BY (FALSEL i)
    end
    handle _ => raise Fail "falseL"

  fun disjR1 (d as ctx ===> p BY _, q) =
    ctx ===> (p \/ q)
    BY (DISJR1 d)

  fun disjR2 (p, d as ctx ===> q BY _) =
    ctx ===> (p \/ q)
    BY (DISJR2 d)

  fun disjL (i, d1 as ctx1 ===> r1 BY _, d2 as ctx2 ===> r2 BY _) =
    let
      val p1 :: ctx1' = ctx1
      val p2 :: ctx2' = ctx2
      val p1' \/ p2' = List.nth (ctx1', i)
      val true = r1 = r2 andalso p1' = p1 andalso p2' = p2 andalso ctx1' = ctx2' 
    in
      ctx1' ===> r1 BY DISJL (i, d1, d2)
    end
    handle _ => raise Fail "disjL"

  fun conjR (d1 as ctx1 ===> p1 BY _, d2 as ctx2 ===> p2 BY _) =
    let
      val true = ctx1 = ctx2
    in
      ctx1 ===> (p1 /\ p2) BY CONJR (d1, d2)
    end
    handle _ => raise Fail "conjR"

  fun conjL1 (i, d as ctx ===> r BY _) =
    let
      val p :: ctx' = ctx
      val p' /\ q = List.nth (ctx, i)
      val true = p = p'
    in
      ctx' ===> r BY CONJL1 (i, d)
    end
    handle _ => raise Fail "conjL1"

  fun conjL2 (i, d as ctx ===> r BY _) =
    let
      val q :: ctx' = ctx
      val p /\ q' = List.nth (ctx, i)
      val true = q = q'
    in
      ctx' ===> r BY CONJL2 (i, d)
    end
    handle _ => raise Fail "conjL2"

  fun implR (d as ctx ===> q BY _) =
    let
      val p :: ctx' = ctx
    in
      ctx' ===> (p ~> q) BY IMPLR d
    end
    handle _ => raise Fail "implR"

  fun implL (i, d1 as ctx ===> p BY _, d2 as ctx' ===> r BY _) =
    let
      val p' ~> q = List.nth (ctx, i)
      val q' :: ctx'' = ctx'
      val true = p = p' andalso q = q' andalso ctx'' = ctx
    in
      ctx ===> r BY IMPLL (i, d1, d2)
    end
    handle _ => raise Fail "implL"

  local
    val rec join = 
      fn [] => ""
       | [x] => x
       | x::xs => x ^ " " ^ join xs

    val sexpr = 
      fn [] => ""
       | [x] => x
       | xs => "(" ^ join xs ^ ")"
  in
    fun prettyTerm m =
      case m of 
         INIT i => sexpr ["init", Int.toString i]
       | TRUER => "trueR"
       | FALSEL i => sexpr ["falseL", Int.toString i]
       | CONJR (d1, d2) => sexpr ["conjR", pretty d1, pretty d2]
       | CONJL1 (i, d) => sexpr ["conjL1", Int.toString i, pretty d]
       | CONJL2 (i, d) => sexpr ["conjL2", Int.toString i, pretty d]
       | DISJR1 d => sexpr ["disjR1", pretty d]
       | DISJR2 d => sexpr ["disjR2", pretty d]
       | DISJL (i, d1, d2) => sexpr ["disjL", Int.toString i, pretty d1, pretty d2]
       | IMPLR d => sexpr ["lam", pretty d]
       | IMPLL (i, d1, d2) => sexpr ["implL", Int.toString i,  pretty d1, pretty d2]
    and pretty d = prettyTerm (out d)
  end
end