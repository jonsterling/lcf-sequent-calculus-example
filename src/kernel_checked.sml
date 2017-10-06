structure CheckedKernel :> KERNEL = 
struct
  structure P = Prop and S = Sequent 

  open P S
  infixr 0 ===>
  infixr 3 ~>
  infixr 4 /\ \/

  (* We represent proofs _ephemerally_ as ML functions, which pass around
     weakenings to capture binding. We could have implemented this as a concrete datatype. *)

  type env = int * context (* A weakening together with a context of assumptions *)
  type proof = env -> prop
  exception InvalidInference

  fun infer ctx m = m (0, ctx)

  fun check (ctx ===> p) m =
    if infer ctx m = p then () else
      raise Fail "Type error"

  fun push (m, n) (i, ctx) = 
    n (i + 1, (m (i, ctx)) :: ctx)

  fun hyp k (i, ctx) = 
    List.nth (ctx, i + k)
    handle _ => raise InvalidInference

  fun unit _ = TRUE

  fun abort (m, p) env = 
    case m env of
       FALSE => p
     | _ => raise InvalidInference

  fun pair (m, n) env =
    m env /\ n env

  fun fst m env = 
    case m env of 
       p /\ q => p
     | _ => raise InvalidInference

  fun snd m env = 
    case m env of 
       p /\ q => q
     | _ => raise InvalidInference

  fun inl (m, p) env = 
    m env \/ p

  fun inr (p, n) env = 
    p \/ n env
  
  fun case_ (m, (n1, n2)) (i, ctx) =
    case m (i, ctx) of
       p \/ q =>
       let
         val r1 = n1 (i + 1, p :: ctx)
         val r2 = n2 (i + 1, q :: ctx)
       in
         if r1 = r2 then r1 else raise InvalidInference
       end
     | _ => raise InvalidInference

  fun lam (p, m) (i, ctx) = 
    p ~> m (i + 1, p :: ctx)

  fun app (m, n) env = 
    case m env of
       p ~> q => if n env = p then q else raise InvalidInference
    | _ => raise InvalidInference
end