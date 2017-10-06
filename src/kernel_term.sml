structure TermData = 
struct
  type hyp = int
  type prop = Prop.prop
  datatype term = 
     hyp of hyp
   | unit
   | abort of term * prop
   | pair of term * term
   | fst of term
   | snd of term
   | inl of term * prop
   | inr of prop * term
   | case_ of term * (term * term)
   | lam of prop * term
   | app of term * term
end

structure TermKernel :
sig
  include KERNEL where type proof = TermData.term
  val pretty : proof -> string
end = 
struct
  structure P = Prop and S = Sequent

  open P S
  infixr 0 ===>
  infixr 3 ~>
  infixr 4 /\ \/

  datatype term = datatype TermData.term
  type proof = term
  
  fun infer ctx m =
    case m of
       hyp i => List.nth (ctx, i)
     | unit => TRUE
     | abort (m, p) => p
     | pair (m, n) => infer ctx m /\ infer ctx n
     | fst m =>
       let
         val p /\ _ = infer ctx m
       in
         p
       end
     | snd m => 
       let
         val _ /\ q = infer ctx m
       in
         q
       end
     | inl (m, q) => infer ctx m \/ q
     | inr (p, m) => p \/ infer ctx m
     | case_ (m, (n1, n2)) =>
       let
         val p \/ q = infer ctx m
         val r = infer (p :: ctx) n1
       in
         check (q :: ctx ===> r) n2;
         r
       end
     | lam (p, m) => p ~> infer (p :: ctx) m
     | app (m, n) => 
       let
         val p ~> q = infer ctx m
       in
         check (ctx ===> p) n;
         q
       end

  and check (ctx ===> p) m =
    if infer ctx m = p then () else
      raise Fail "Type error"

  fun lift m = 
    case m of 
       hyp i => hyp (i + 1)
     | unit => unit
     | abort (n, p) => abort (lift n, p)
     | pair (m, n) => pair (lift m, lift n)
     | fst m => fst (lift m)
     | snd m => snd (lift m)
     | inl (m, q) => inl (lift m, q)
     | inr (p, m) => inr (p, lift m)
     | case_ (m, (n1, n2)) => case_ (lift m, (lift n1, lift n2))
     | lam (p, m) => lam (p, lift m)
     | app (m, n) => app (lift m, lift n)
    
  fun push (m, n) = 
    case n of 
       hyp 0 => m
     | hyp j => hyp (j - 1)
     | unit => unit
     | abort (n, p) => abort (push (m, n), p)
     | pair (n1, n2) => pair (push (m, n1), push (m, n2))
     | fst n => fst (push (m, n))
     | snd n => snd (push (m, n))
     | inl (n, q) => inl (push (m, n), q)
     | inr (p, n) => inr (p, push (m, n))
     | case_ (l, (n1, n2)) =>
       let
         val l' = lift l
       in
         case_ (push (m, l), (push (l', n1), push (l', n2)))
       end
     | lam (p, n) => lam (p, push (lift m, n))
     | app (n1, n2) => app (push (m, n1), push (m, n2))


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
    val rec pretty = 
      fn hyp i => sexpr ["hyp", Int.toString i]
       | unit => "unit"
       | abort (n, p) => sexpr ["abort", pretty n]
       | pair (m, n) => sexpr ["pair", pretty m, pretty n]
       | fst m => sexpr ["fst", pretty m]
       | snd m => sexpr ["snd", pretty m]
       | inl (m, q) => sexpr ["inl", pretty m]
       | inr (p, m) => sexpr ["inr", pretty m]
       | case_ (m, (n1, n2)) => sexpr ["case", pretty m, pretty n1, pretty n2]
       | lam (p, m) => sexpr ["lam", pretty m]
       | app (m, n) => sexpr ["app", pretty m, pretty n]
  end
end