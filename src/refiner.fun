functor Refiner (K : KERNEL) : REFINER = 
struct
  structure K = K
  open K.S K.P

  infixr 0 ===>
  infixr 3 ~>
  infixr 4 /\ \/

  type goal = sequent
  type subgoals = goal list
  type validation = K.proof list -> K.proof
  type state = subgoals * validation
  type tactic = goal -> state

  val trueR = 
    fn ctx ===> TRUE => ([], fn rho => K.unit)
     | _ => raise Fail "trueR"

  fun falseL i (ctx ===> p) =
    ([], fn rho => K.abort (K.hyp i, p))

  val conjR = 
    fn ctx ===> p /\ q =>
       ([ctx ===> p, ctx ===> q],
        fn [m1, m2] => K.pair (m1, m2))
     | _ => raise Fail "conjR"

  fun conjL1 i (ctx ===> r) =
    let
      val p /\ _ = List.nth (ctx, i)
    in
      ([p :: ctx ===> r],
       fn [m] => K.push (K.fst (K.hyp i), m))
    end

  fun conjL2 i (ctx ===> r) = 
    let
      val _ /\ q = List.nth (ctx, i)
    in
      ([q :: ctx ===> r],
       fn [m] => K.push (K.snd (K.hyp i), m))
    end

  val disjR1 = 
    fn ctx ===> p \/ q => 
       ([ctx ===> p],
        fn [m] => K.inl (m, q))
     | _ => raise Fail "disjR1"

  val disjR2 = 
    fn ctx ===> p \/ q => 
       ([ctx ===> q],
        fn [m] => K.inr (p, m))
     | _ => raise Fail "disjR2"

  fun disjL i (ctx ===> r) = 
    let
      val p \/ q = List.nth (ctx, i)
    in
      ([p :: ctx ===> r, q :: ctx ===> r],
       fn [m1, m2] => K.case_ (K.hyp i, (m1, m2)))
    end

  val implR = 
    fn ctx ===> p ~> q =>
       ([p :: ctx ===> q],
        fn [m] => K.lam (p, m))
     | _ => raise Fail "implR"

  fun implL i (ctx ===> r) = 
    let
      val p ~> q = List.nth (ctx, i)
    in
      ([ctx ===> p, q :: ctx ===> r],
       fn [m1, m2] => K.push (K.app (K.hyp i, m1), m2))
    end
end