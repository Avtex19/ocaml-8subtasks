let rec member c t l =
  match l with
  | [] -> false
  | hd :: tl -> if c t hd = 0 then true else member c t tl


  let rec count_occurrences list1 = 
    match list1 with 
    | [] -> []
    | x::xs -> let count = List.filter(fun a-> a=x) list1 |> List.length in
    let res = count_occurrences((List.filter(fun a-> a <> x) xs)) in
    List.sort(fun(_,count1) (_,count2) -> compare count2 count1) ((x,count) :: res)
  

let rec drop_last list1 = 
  match list1 with 
  | [] -> failwith "Empty list has no last element"
  | [x] -> []
  | x::xs -> x::drop_last xs

  

let rec drop_last_opt list1 = 
  match list1 with 
  | [] -> None
  | [x] -> Some []
  | x::xs -> match drop_last_opt xs with 
                  | Some a -> Some (x::a) (* if some a is in xs, then we add it into a list with x(head)*)
                  | None -> None


            
let zip_with f list1 list2 =
  let rec loop acc i =
    if i >= List.length list1 || i >= List.length list2 then
      List.rev acc
    else
      let x = f (List.nth list1 i) (List.nth list2 i) in 
      loop (x :: acc) (i+1)
  in
  loop [] 0;; 
  
let unzip lst =
    List.fold_left (fun (acc1, acc2) (x, y) -> (x::acc1, y::acc2)) ([], []) lst 
    |> fun (list1, list2) -> (List.rev list1, List.rev list2)

    (* Our input is [('a',1); ('b',2)]. Our function List.fold_left is called with arguments which are the accumulator function
       (fun (acc1, acc2) (x,y) -> (x::acc1,y::acc2)), accumulator value in the beggining is ([],[])
       and input list is which I mentioned above. The first element of the input list is ('a',1) and current accumulator value is ([],[]). The function returns the new accumulator value which is 
       (['a'], [1]). The second element of the input is ('b',2) and our accumulator function is called now with the current accumulator value which is  
       (['a'], [1]) and the current element ('b',2). Our function returns the new accumulator value which is (['b'; 'a'], [2; 1]). Then 
       |> is used to chain List.fold_left and List.rev functions, so the result in pair of reversed lists is  (['a'; 'b'], [1; 2]). And last, our unzip function
       gives us result which is (['a'; 'b'], [1; 2]) and it is our output.*)

let table_and_scorers lst  =
     let games_played team lst = 
            
  List.filter(fun(t1,_,t2,_) -> t1 = team || t2 = team) lst |> List.length 
     in 
     let games_won team lst = 
      List.filter(fun(t1,g1,t2,g2) -> (t1 = team && List.length g1 > List.length g2) || (t2 = team && List.length g2 > List.length g1)) lst |> List.length
    in 
    let games_draw team lst = 
      List.filter(fun(t1,g1,t2,g2) -> (t1 = team && List.length g1 = List.length g2) || (t2 = team && List.length g2 = List.length g1)) lst |> List.length
    in
    let games_lost team lst = 
              List.filter(fun(t1,g1,t2,g2) -> (t1 = team && List.length g1 < List.length g2) || (t2 = team && List.length g2 < List.length g1)) lst |> List.length
            in
    let goals_scored team lst = 
            lst
            |> List.filter(fun(t1,g1,t2,g2) -> t1 = team || t2 = team) 
            |> List.map(fun(t1,g1,t2,g2) -> if t1 = team then List.length g1 else List.length g2)
            |> List.fold_left (+) 0 
    in


    let goals_conceded team lst = 
      lst
      |> List.filter(fun(t1,g1,t2,g2) -> t1 = team || t2 = team) 
      |> List.map(fun(t1,g1,t2,g2) -> if t1 = team then List.length g2 else List.length g1)
      |> List.fold_left (+) 0 
    in


    let team_points team lst =
      let won = games_won team lst in 
      let drawn = games_draw team lst in 
      let points = (won * 3) + drawn in
      (points)
    in
    
        let rec create_statistic acc mainL = match mainL with 
        | [] -> acc
        | (t1, g1, t2, g2)::xs -> create_statistic 
        (acc@[(t1, games_played t1 lst,games_won t1 lst,games_draw t1 lst,games_lost t1 lst,goals_scored t1 lst,goals_conceded t1 lst,team_points t1 lst); 
        (t2, games_played t2 lst,games_won t2 lst,games_draw t2 lst,games_lost t2 lst,goals_scored t2 lst,goals_conceded t2 lst,team_points t2 lst)]) xs

  in let result =  create_statistic [] lst in

    let rec filterMyResult resultL acc = match resultL with
        | [] -> acc
        | x::xs -> if (List.length (List.filter (fun cvlad -> x = cvlad) resultL)) = 1 then 
        filterMyResult xs (acc@[x])
        else filterMyResult xs (acc)
        in
        let unsorted_result = filterMyResult result [] in
        let final_stats = List.sort (fun (t1, g1, w1, d1, l1, gf1, ga1, p1)(t2, g2, w2, d2, l2, gf2, ga2, p2)->
          if p1 > p2 then -1 else if p1 < p2 then 1
          else if gf1-ga1 > gf2-ga2 then -1 else if gf1-ga1 > gf2-ga2 then 1 else if gf1 > gf2 then -1 else if gf1 < gf2 then 1
          else 0) unsorted_result 
        in
          let scorers lst = 
            let each_count_scorer team name lst =
              lst
              |> List.filter(fun(t1,g1,t2,g2) -> t1 = team || t2 = team) 
              |> List.map(fun(t1,g1,t2,g2) -> if t1=team then (g1) else (g2))
              |> List.fold_left (@) []
              |> List.filter(fun(x) -> x = name)
              |> List.length
          
          
            in
          
          
          
            let rec countActualScorers gamesL acc = match gamesL with
            | [] -> acc
            | (t1,g1,t2,g2)::xs -> 
              let mapEachPlayer = List.map (fun x -> (x, t1, each_count_scorer  t1 x lst)) g1 in
              let mapEachPlayer2 = List.map (fun x -> (x, t2, each_count_scorer t2 x lst)) g2
              in countActualScorers xs (acc@[mapEachPlayer]@[mapEachPlayer2])
          
              in 
          
              List.sort 
              (fun (name1, team1, score1) (name2, team2, score2) -> 
              if score1 > score2 then -1 
              else if score1 < score2 then 1 
              else if name1 < name2 then -1 
              else if name1 > name2 then 1 else 0
              )
              (filterMyResult (List.fold_left (@) [] (List.filter (fun x -> x !=[]) (countActualScorers lst []))) [])
            in let final_scorers = scorers lst in (final_stats,final_scorers);;
    
      