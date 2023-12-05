open Core

let year = 2023
let day = 2

module Part_1 = struct
  type game_set =
    { r : int
    ; g : int
    ; b : int
    }

  let run input =
    Ok
      (List.reduce_exn
         ~f:( + )
         (List.mapi (String.split_lines input) ~f:(fun i line ->
            let game_sets_str = List.nth_exn (String.split line ~on:':') 1 in
            let game_sets_sep = String.split game_sets_str ~on:';' in
            let max_game = { r = 12; g = 13; b = 14 } in
            let game_sets =
              List.map game_sets_sep ~f:(fun st ->
                List.fold
                  ~init:{ r = 0; g = 0; b = 0 }
                  ~f:(fun acc num_color ->
                    let num_color = String.split num_color ~on:' ' in
                    match List.last_exn num_color with
                    | "red" -> { acc with r = Int.of_string (List.hd_exn num_color) }
                    | "green" -> { acc with g = Int.of_string (List.hd_exn num_color) }
                    | "blue" -> { acc with b = Int.of_string (List.hd_exn num_color) }
                    | _ -> acc)
                  (List.map (String.split st ~on:',') ~f:(fun s -> String.strip s)))
            in
            if List.exists game_sets ~f:(fun g ->
                 g.r > max_game.r || g.g > max_game.g || g.b > max_game.b)
            then 0
            else i + 1))
       |> Int.to_string)
  ;;
end

module Part_2 = struct
  type game_set =
    { r : int
    ; g : int
    ; b : int
    }

  let run input =
    Ok
      (List.reduce_exn
         ~f:( + )
         (List.map (String.split_lines input) ~f:(fun line ->
            let game_sets_str = List.nth_exn (String.split line ~on:':') 1 in
            let game_sets_sep = String.split game_sets_str ~on:';' in
            let game_sets =
              List.map game_sets_sep ~f:(fun st ->
                List.fold
                  ~init:{ r = 0; g = 0; b = 0 }
                  ~f:(fun acc num_color ->
                    let num_color = String.split num_color ~on:' ' in
                    match List.last_exn num_color with
                    | "red" -> { acc with r = Int.of_string (List.hd_exn num_color) }
                    | "green" -> { acc with g = Int.of_string (List.hd_exn num_color) }
                    | "blue" -> { acc with b = Int.of_string (List.hd_exn num_color) }
                    | _ -> acc)
                  (List.map (String.split st ~on:',') ~f:(fun s -> String.strip s)))
            in
            List.reduce_exn game_sets ~f:(fun c' c ->
              { r = (if c'.r > c.r then c'.r else c.r)
              ; g = (if c'.g > c.g then c'.g else c.g)
              ; b = (if c'.b > c.b then c'.b else c.b)
              })
            |> (fun g -> g.r * g.g * g.b)))
       |> Int.to_string)
  ;;
end
