functor Tactic (R : REFINER) : TACTIC = 
struct
  open R

  type tactic = goal -> subgoals * validation

  fun splitAt (xs, i) =
    let
      val front = List.take (xs, i)
      val back = List.drop (xs, i)
    in
      (front, back)
    end

  fun gobbleWith ([], []) args = []
    | gobbleWith (n :: ns, f :: fs) args = 
      let
        val (xs, args') = splitAt (args, n)
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