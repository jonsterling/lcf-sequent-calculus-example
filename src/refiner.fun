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
    let
      val true = List.nth (ctx, i) = p
    in
      ([], fn [] => K.init (ctx, i))
    end
    handle _ => raise Fail "init"

  fun trueR (ctx ===> p) = 
    let
      val TRUE = p
    in
      ([], fn [] => K.trueR ctx)
    end
    handle _ => raise Fail "trueR"

  fun falseL i (ctx ===> p) =
    let
      val FALSE = List.nth (ctx, i)
    in
      ([], fn [] => K.falseL (i, ctx ===> p))
    end
    handle _ => raise Fail "falseL"

  fun conjR (ctx ===> r)  = 
    let
      val p /\ q = r
    in
      ([ctx ===> p, ctx ===> q],
       fn [d1, d2] => K.conjR (d1, d2))
    end
    handle _ => raise Fail "conjR"

  fun conjL1 i (ctx ===> r) =
    let
      val p /\ _ = List.nth (ctx, i)
    in
      ([p :: ctx ===> r],
       fn [d] => K.conjL1 (i, d))
    end
    handle _ => raise Fail "conjL1"

  fun conjL2 i (ctx ===> r) = 
    let
      val _ /\ q = List.nth (ctx, i)
    in
      ([q :: ctx ===> r],
       fn [d] => K.conjL2 (i, d))
    end

  fun disjR1 (ctx ===> r) = 
    let
      val p \/ q = r
     in
       ([ctx ===> p],
        fn [d] => K.disjR1 (d, q))
     end
     handle _ => raise Fail "disjR1"

  fun disjR2 (ctx ===> r) = 
    let
      val p \/ q = r
     in
       ([ctx ===> q],
        fn [d] => K.disjR2 (p, d))
     end
     handle _ => raise Fail "disjR2"

  fun disjL i (ctx ===> r) = 
    let
      val p \/ q = List.nth (ctx, i)
    in
      ([p :: ctx ===> r, q :: ctx ===> r],
       fn [d1, d2] => K.disjL (i, d1, d2))
    end

  fun implR (ctx ===> r) = 
    let
      val p ~> q = r
    in
      ([p :: ctx ===> q],
       fn [d] => K.implR d)
    end
    handle _ => raise Fail "implR"

  fun implL i (ctx ===> r) = 
    let
      val p ~> q = List.nth (ctx, i)
    in
      ([ctx ===> p, q :: ctx ===> r],
       fn [d1, d2] => K.implL (i, d1, d2))
    end
    handle _ => raise Fail "implL"
end

structure Refiner = Refiner (Kernel)