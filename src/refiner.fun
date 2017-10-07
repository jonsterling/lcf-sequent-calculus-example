functor Refiner (K : KERNEL) : REFINER = 
struct
  structure K = K
  open K.S K.P

  infixr 0 ===>
  infixr 3 ~>
  infixr 4 /\ \/

  type goal = sequent
  type subgoals = goal list
  type proof = K.proof
  type validation = proof list -> proof
  type rule = goal -> subgoals * validation

  val prettyGoal = Sequent.pretty

  fun init i (ctx ===> p) =
    if List.nth (ctx, i) = p then 
      ([], fn rho => K.init (ctx, i))
    else
      raise Fail "init"

  val trueR = 
    fn ctx ===> TRUE => ([], fn rho => K.trueR ctx)
     | _ => raise Fail "trueR"

  fun falseL i (ctx ===> p) =
    ([], fn rho => K.falseL (ctx, i,p))

  val conjR = 
    fn ctx ===> p /\ q =>
       ([ctx ===> p, ctx ===> q],
        fn [d1, d2] => K.conjR (d1, d2))
     | _ => raise Fail "conjR"

  fun conjL1 i (ctx ===> r) =
    let
      val p /\ _ = List.nth (ctx, i)
    in
      ([p :: ctx ===> r],
       fn [d] => K.conjL1 (i, d))
    end

  fun conjL2 i (ctx ===> r) = 
    let
      val _ /\ q = List.nth (ctx, i)
    in
      ([q :: ctx ===> r],
       fn [d] => K.conjL2 (i, d))
    end

  val disjR1 = 
    fn ctx ===> p \/ q => 
       ([ctx ===> p],
        fn [d] => K.disjR1 (d, q))
     | _ => raise Fail "disjR1"

  val disjR2 = 
    fn ctx ===> p \/ q => 
       ([ctx ===> q],
        fn [d] => K.disjR2 (p, d))
     | _ => raise Fail "disjR2"

  fun disjL i (ctx ===> r) = 
    let
      val p \/ q = List.nth (ctx, i)
    in
      ([p :: ctx ===> r, q :: ctx ===> r],
       fn [d1, d2] => K.disjL (i, d1, d2))
    end

  val implR = 
    fn ctx ===> p ~> q =>
       ([p :: ctx ===> q],
        fn [d] => K.implR d)
     | _ => raise Fail "implR"

  fun implL i (ctx ===> r) = 
    let
      val p ~> q = List.nth (ctx, i)
    in
      ([ctx ===> p, q :: ctx ===> r],
       fn [d1, d2] => K.implL (i, d1, d2))
    end
end

structure Refiner = Refiner (Kernel)