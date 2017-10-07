structure ListUtil :
sig
  val splitAt : 'a list * int -> 'a list * 'a list
end =
struct
  fun splitAt (xs, i) = 
    let
      val rec loop = 
        fn (0, ac, xs) => (List.rev ac, xs)
         | (n, ac, []) => raise Empty
         | (n, ac, x::xs) => loop (i - 1, x :: ac, xs)
    in
      loop (i, [], xs)
    end
end

functor Tactic (R : REFINER) : TACTIC = 
struct
  open R

  type tactic = goal -> subgoals * validation

  fun gobbleWith ([], []) args = []
    | gobbleWith (n :: ns, f :: fs) args = 
      let
        val (xs, args') = ListUtil.splitAt (args, n)
      in
        f xs :: gobbleWith (ns, fs) args'
      end

  fun stitchProof (validation, subgoalss, validations) =
    (List.concat subgoalss,
      validation o
        gobbleWith (map length subgoalss, validations))


  fun then_ (t1, t2) goal =
    let
      val (subgoals, validation) = t1 goal
      val (subgoalss, validations) = ListPair.unzip (List.map t2 subgoals)
    in
      stitchProof (validation, subgoalss, validations)
    end

  fun thenl (t, ts) goal =
    let
      val (subgoals, validation) = t goal
      val (subgoalss, validations) = ListPair.unzip (ListPair.mapEq (fn (t, g) => t g) (ts, subgoals))
    in
      stitchProof (validation, subgoalss, validations)
    end

  fun orelse_ (t1, t2) (goal : goal) =
    t1 goal handle _ => t2 goal

  fun id goal = ([goal], fn [d] => d)
  fun fail _ = raise Fail "Fail!"
  fun complete t = then_ (t, fail)

  fun hole msg goal = 
    (print (msg ^ ":   " ^ R.prettyGoal goal);
     id goal)
end