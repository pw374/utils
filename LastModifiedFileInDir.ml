(* Last modified file in directory *)
(* Licence: ISC *)
(* (c) 2014 Philippe Wang <philippe.wang@gmail.com> *)
open Printf

let rec find_latest_file dir =
  let open Unix in
  if (stat dir).st_kind <> S_DIR then
    dir
  else
    let opendir d = try Some(opendir d) with _ -> None in
    let readdir = function
      | None -> None
      | Some d -> try Some(readdir d) with End_of_file -> None in
    let stat_dummy =
      { (stat Sys.argv.(0)) with st_kind = S_BLK; st_mtime = 0.0; } in
    let d = opendir dir in
    let rec loop (latest, latest_date) =
      let rec aux x =
        let sx = try stat x with _ -> stat_dummy in
        match sx.st_kind with
        | S_REG when sx.st_mtime > latest_date ->
          x, sx.st_mtime
        | S_DIR ->
          aux (find_latest_file x)
        | _ ->
          latest, latest_date
      in
      match readdir d with
      | Some ("." | "..") -> loop (latest, latest_date)
      | None -> latest
      | Some x -> loop (aux (Filename.concat dir x))
    in
    loop ("", 0.0)

let _ = print_endline (find_latest_file Sys.argv.(1))
