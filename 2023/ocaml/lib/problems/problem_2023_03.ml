open Core

let year = 2023
let day = 3

module Part_1 = struct
  type num = 
    { value : int;
      len : int;
    }
  type token_type =
    | Num of num
    | Symbol
  type token =
    { idx : int;
      t_type : token_type;
    }

  let print_token_list ts =
    List.iter ts ~f:(fun t -> 
      Printf.printf "Token: { idx = %d; t_type = %s; }\n" t.idx
      (match t.t_type with
        | Symbol -> "Symbol"
        | Num n -> Format.sprintf "Num { value = %d; len = %d; }" n.value n.len))

  let lex st =
    String.foldi st ~init:[]
    ~f:(fun i out c -> 
      if not (Char.is_digit c) && not Char.(c = '.') 
        then out @ [ { idx = i; t_type = Symbol } ]
      else if Char.is_digit c 
        then 
          if i > 0 && Char.is_digit st.[i - 1] 
            then out
          else out @ [ (
            String.fold_until (String.drop_prefix st i) 
            ~init:{ idx = i; t_type = Num { value = 0; len = 0; }; }
            ~f:(fun acc n -> if not (Char.is_digit n) then Stop acc
              else let (v, l) = match acc.t_type with 
                | Num num -> (num.value, num.len)
                | Symbol -> (0, 0) in
              Continue { 
                acc with t_type = Num { 
                  value = v * 10 + (Char.get_digit_exn n); 
                  len = l + 1; 
                }; 
              })
            ~finish:(fun acc -> acc)
          ) ]
      else out)

  let gen_surrounding_indeces num idx ~row_len =
    (* left *)
    (if ((idx - 1) mod row_len) = row_len - 1 then [] else 
      [ idx - 1; idx - row_len - 1; idx + row_len - 1 ]) ::
    (* right *)
    (if ((idx + num.len) mod row_len) = 0 then [] else 
      [ idx + num.len; idx - row_len + num.len; idx + row_len + num.len ]) ::
    (* up (no bounds checking needed in this case) *)
    List.init num.len ~f:(fun i -> idx - row_len + i) ::
    (* down (no bounds checking needed in this case) *)
    List.init num.len ~f:(fun i -> idx + row_len + i) ::
    [] |> List.join

  let is_part_num num idx ~row_len ~tokens =
    let symbol_indeces = 
      List.filter_map tokens ~f:(fun t -> 
        match t.t_type with
          | Symbol -> Some t.idx
          | Num _ -> None)
    in
    List.exists (gen_surrounding_indeces num idx ~row_len) 
    ~f:(fun i -> List.mem symbol_indeces i ~equal:( = ))

  let run input =
    let row_len = 
      Option.value_exn (String.lfindi input ~f:(fun _ c -> Char.(c = '\n')))
    in
    Ok (
      let tokens = String.filter input ~f:(fun c -> not Char.(c = '\n')) |> lex in
      print_token_list tokens;
      (List.fold tokens ~init:0 ~f:(fun acc t -> 
        acc + match t.t_type with 
          | Symbol -> 0 
          | Num num -> if is_part_num num t.idx ~row_len ~tokens then (Printf.printf "%d\n" num.value; num.value) else 0))
      |> Int.to_string
    )
end

module Part_2 = struct
  type num = 
    { value : int;
      len : int;
    }
  type token_type =
    | Num of num
    | PotentialGear
  type token =
    { idx : int;
      t_type : token_type;
    }

  let print_token_list ts =
    List.iter ts ~f:(fun t -> 
      Printf.printf "Token: { idx = %d; t_type = %s; }\n" t.idx
      (match t.t_type with
        | PotentialGear -> "PotentialGear"
        | Num n -> Format.sprintf "Num { value = %d; len = %d; }" n.value n.len))

  let lex st =
    String.foldi st ~init:[]
    ~f:(fun i out c -> 
      if Char.(c = '*') 
        then out @ [ { idx = i; t_type = PotentialGear } ]
      else if Char.is_digit c 
        then 
          if i > 0 && Char.is_digit st.[i - 1] 
            then out
          else out @ [ (
            String.fold_until (String.drop_prefix st i) 
            ~init:{ idx = i; t_type = Num { value = 0; len = 0; }; }
            ~f:(fun acc n -> if not (Char.is_digit n) then Stop acc
              else let (v, l) = match acc.t_type with 
                | Num num -> (num.value, num.len)
                | PotentialGear -> (0, 0) in
              Continue { 
                acc with t_type = Num { 
                  value = v * 10 + (Char.get_digit_exn n); 
                  len = l + 1; 
                }; 
              })
            ~finish:(fun acc -> acc)
          ) ]
      else out)

  let gen_surrounding_indeces idx ~row_len =
    (* left *)
    (if ((idx - 1) mod row_len) = row_len - 1 then [] else 
      [ idx - 1; idx - row_len - 1; idx + row_len - 1 ]) ::
    (* right *)
    (if ((idx + 1) mod row_len) = 0 then [] else 
      [ idx + 1; idx - row_len + 1; idx + row_len + 1 ]) ::
    (* up and down (no bounds checking needed in this case) *)
    [ idx - row_len; idx + row_len ] ::
    [] |> List.join

  let idx_in_num_token idx t = 
    match t.t_type with
      | PotentialGear -> false
      | Num num -> List.exists ~f:(fun idx' -> idx = idx') 
        (List.init num.len ~f:(fun i -> t.idx + i))

  let get_gear_ratio gear_idx ~in_ ~row_len =
    let surrounding = gen_surrounding_indeces gear_idx ~row_len in
    let adj = List.filter_map in_ 
    ~f:(fun t -> match t.t_type with
      | PotentialGear -> None
      | Num num -> if List.exists surrounding ~f:(fun i -> idx_in_num_token i t)
        then Some num else None) in
    if List.length adj = 2 
    then (List.nth_exn adj 0).value * (List.nth_exn adj 1).value else 0

  let run input =
    let row_len = 
      Option.value_exn (String.lfindi input ~f:(fun _ c -> Char.(c = '\n')))
    in
    Ok (
      let tokens = String.filter input ~f:(fun c -> not Char.(c = '\n')) |> lex in
      print_token_list tokens;
      (List.fold tokens ~init:0 ~f:(fun acc t -> 
        acc + match t.t_type with 
          | Num _ -> 0
          | PotentialGear -> get_gear_ratio t.idx ~in_:tokens ~row_len))
      |> Int.to_string
    )
end
