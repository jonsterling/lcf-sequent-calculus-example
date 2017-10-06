structure Kernel :> KERNEL =
struct
  structure P = Prop and S = Sequent

  open P S
  infixr 0 ===> !-
  infixr 3 ~>
  infixr 4 /\ \/

  datatype judgment = !- of context * prop
  datatype term = datatype TermData.term
  datatype proof = BY of judgment * proof term
  infix BY

  fun out (_ BY m) = m

  fun infer _ = raise Match
  fun check _ = raise Match

  fun hyp (ctx, i) =
    ctx !- List.nth (ctx, i)
    BY HYP i

  fun unit ctx =
    ctx !- TRUE
    BY UNIT
  
  fun abort (d as ctx !- _ BY _, p) =
    ctx !- p
    BY (ABORT d)

  fun inl (d as ctx !- p BY _, q) =
    ctx !- (p \/ q)
    BY (INL d)

  fun inr (p, d as ctx !- q BY _) =
    ctx !- (p \/ q)
    BY (INR d)

  fun case_ (d as ctx !- p BY _, (e1 as ctx1 !- r1 BY _, e2 as ctx2 !- r2 BY _)) =
    case p of 
       p1 \/ p2 => 
       if ctx = ctx1 andalso ctx = ctx2 andalso r1 = r2 then 
         ctx !- r1
         BY (CASE (d, (e1, e2)))
       else
         raise Fail "case_"
     | _ => raise Fail "case_"
    

  fun pair (d1 as ctx1 !- p1 BY _, d2 as ctx2 !- p2 BY _) =
    if ctx1 = ctx2 then
      ctx1 !- (p1 /\ p2)
      BY PAIR (d1, d2)
    else
      raise Fail "pair"

  fun fst (d as ctx !- p BY _) =
    case p of
       p1 /\ p2 => ctx !- p1 BY FST d
     | _ => raise Fail "fst"

  fun snd (d as ctx !- p BY _) =
    case p of 
       p1 /\ p2 => ctx !- p2 BY SND d
     | _ => raise Fail "snd"

  fun lam (d as ctx !- q BY _) =
    case ctx of
       p :: ctx' => ctx !- (p ~> q) BY LAM d
     | _ => raise Fail "lam"

  fun app (d1 as ctx1 !- rq BY _, d2 as ctx2 !- p BY _) =
    case rq of
       r ~> q =>
       if r = p andalso ctx1 = ctx2 then 
         ctx1 !- q BY APP (d1, d2)
       else
         raise Fail "app"
     | _ => raise Fail "app"

  local
    fun liftTerm p =
      fn HYP i => HYP (i + 1)
       | UNIT => UNIT
       | ABORT d => ABORT (lift p d)
       | PAIR (d1, d2) => PAIR (lift p d1, lift p d2)
       | FST d => FST (lift p d)
       | SND d => SND (lift p d)
       | INL d => INL (lift p d)
       | INR d => INR (lift p d)
       | CASE (d, (e1, e2)) => CASE (lift p d, (lift p e1, lift p e2))
       | LAM d => LAM (lift p d)
       | APP (d1, d2) => APP (lift p d1, lift p d2)

    and lift p (ctx !- q BY m) = p :: ctx !- q BY liftTerm p m
      
  in
    fun push (d, (jdg as q :: ctx !- p) BY m) =
      case m of 
         HYP 0 =>
         let
           val jdg' BY m' = d
         in
           if jdg' = (ctx !- p) then d else raise Fail "Ill-typed substitution"
         end
       | HYP i => ctx !- p BY HYP (i - 1)
       | UNIT => ctx !- p BY UNIT
       | PAIR (e1, e2) => ctx !- p BY PAIR (push (d, e1), push (d, e2))
       | FST e => ctx !- p BY FST (push (d, e))
       | SND e => ctx !- p BY SND (push (d, e))
       | INL e => ctx !- p BY INL (push (d, e))
       | INR e => ctx !- p BY INR (push (d, e))
       | CASE (e, (f1, f2)) => 
         let
           val _ !- q1 \/ q2 BY _ = e
           val d1 = lift q1 d
           val d2 = lift q2 d
         in
           ctx !- p BY CASE (push (d, e), (push (d1, f1), push (d2, f2)))
         end
       | LAM e =>
         let
           val r ~> _ = p
         in
           ctx !- p BY LAM (push (lift r d, e))
         end
       | APP (e1, e2) => ctx !- p BY APP (push (d, e1), push (d, e2))
  end

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
         HYP i => sexpr ["hyp", Int.toString i]
       | UNIT => "unit"
       | ABORT d => sexpr ["abort", pretty d]
       | PAIR (d1, d2) => sexpr ["pair", pretty d1, pretty d2]
       | FST d => sexpr ["fst", pretty d]
       | SND d => sexpr ["snd", pretty d]
       | INL d => sexpr ["inl", pretty d]
       | INR d => sexpr ["inr", pretty d]
       | CASE (d, (e1, e2)) => sexpr ["case", pretty d, pretty e1, pretty e2]
       | LAM d => sexpr ["lam", pretty d]
       | APP (d1, d2) => sexpr ["app", pretty d1, pretty d2]
    and pretty d = prettyTerm (out d)
  end
end