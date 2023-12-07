open Core

let year = 2023
let day = 4

module Part_1 = struct
  let run input =
    Ok
      (List.fold (String.split_lines input) ~init:0 ~f:(fun acc line ->
         let lists_str = List.last_exn (String.split line ~on:':') in
         let lists = String.split lists_str ~on:'|' in
         let lists =
           List.map lists ~f:(fun s ->
             String.strip s
             |> String.split ~on:' '
             |> List.filter_map ~f:(fun s ->
               if String.is_empty s then None else Some (Int.of_string s)))
         in
         let intersection =
           List.filter (List.hd_exn lists) ~f:(fun e ->
             List.mem (List.last_exn lists) e ~equal:( = ))
         in
         acc
         +
         match intersection with
         | [] -> 0
         | _ -> Int.pow 2 (List.length intersection - 1))
       |> Int.to_string)
  ;;
end

module Part_2 = struct
  let run input = Ok input
end
