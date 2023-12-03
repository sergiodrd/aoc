open Base

let year = 2023
let day = 1

module Part_1 = struct
  let get_line_calibration line =
    let first = String.find ~f:Char.is_digit line in
    let last = String.find ~f:Char.is_digit (String.rev line) in
    match (first, last) with
    | None, None -> 0
    | Some f, Some l -> (Char.get_digit_exn f * 10) + Char.get_digit_exn l
    | _ -> -1

  let run input =
    let total =
      List.map (String.split_lines input) ~f:get_line_calibration |> List.reduce ~f:( + )
    in
    Ok (match total with None -> "0" | Some x -> Int.to_string x)
end

module Part_2 = struct
  let get_line_calibration_part1 line =
    let first = String.find ~f:Char.is_digit line in
    let last = String.find ~f:Char.is_digit (String.rev line) in
    match (first, last) with
    | None, None -> 0
    | Some f, Some l -> (Char.get_digit_exn f * 10) + Char.get_digit_exn l
    | _ -> -1

  let get_line_calibration line =
    let words = [ "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"; ] in
    let replacements = [ "o1e"; "t2o"; "t3e"; "f4r"; "f5e"; "s6x"; "s7n"; "e8t"; "n9e"; ] in
    List.foldi words ~init:line ~f:(fun i acc w -> String.substr_replace_all acc ~pattern:w ~with_:(List.nth_exn replacements i))
    |> get_line_calibration_part1

  let run input =
    let total =
      List.map (String.split_lines input) ~f:get_line_calibration |> List.reduce ~f:( + )
    in
    Ok (match total with None -> "0" | Some x -> Int.to_string x)
end
