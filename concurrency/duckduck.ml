open Core
open Async

let query_uri ~server ~query =
  let base_uri =
    Uri.of_string (String.concat ["http://"; server; "/?format=json"])
  in
  let res = Uri.add_query_param base_uri ("q", [query]) in
  printf "%s\n" (Uri.to_string res);
  res

let get_definition_from_json json =
  match Yojson.Safe.from_string json with
  | `Assoc kv_list -> (
      let find key =
        match List.Assoc.find ~equal:String.equal kv_list key with
        | None | Some (`String "") -> None
        | Some s -> Some (Yojson.Safe.to_string s)
      in
      match find "Abstract" with Some _ as x -> x | None -> find "Definition")
  | _ -> None

let get_definition ~server ~interrupt ~word =
  try_with (fun () ->
      Cohttp_async.Client.get ~interrupt (query_uri ~server ~query:word)
      >>= fun (_, body) ->
      Cohttp_async.Body.to_string body >>| fun string ->
      (word, get_definition_from_json string))
  >>| function
  | Ok (word, result) -> (word, Ok result)
  | Error _ -> (word, Error "Unexpected failure")

let get_definition_with_timeout ~server ~timeout ~word =
  let interrupt = Ivar.create () in
  choose [
    choice (after timeout) (fun () ->
        Ivar.fill interrupt ();
        (word, Error "Timed out"));
    choice (get_definition ~server ~interrupt:(Ivar.read interrupt) ~word)
      (fun (word, result) ->
         let result' =
           match result with
           | Ok _ as x -> x
           | Error _ -> Error "Unexpected failure"
         in
         (word, result'));
  ]

let print_result (word, definition) =
  printf "%s\n%s\n\n%s\n\n" word
    (String.init (String.length word) ~f:(fun _ -> '-'))
    (match definition with
     | Error s -> "DuckDuckGo query BAD: " ^ s
     | Ok None -> "No definition found"
     | Ok (Some def) ->
       String.concat ~sep:"\n" (Wrapper.wrap (Wrapper.make 70) def))

let search_and_print servers timeout words =
  Deferred.any (List.map servers ~f:(fun server ->
      Deferred.all_unit (List.map words ~f:(fun word ->
          get_definition_with_timeout ~server ~timeout ~word >>| print_result));
    ))

let () =
  Command.async ~summary:"DuckDuckGo searcher"
    Command.Let_syntax.(
      let%map_open words = anon (sequence ("word" %: string))
      and timeout = flag "-t" (required float) ~doc:" timeout for request"
      (*and servers =
        flag "-s" (listed string) ~doc:" list of servers"*)
      in
      fun () ->
        let timeout = Time.Span.of_sec timeout in
        let servers = ["api.duckduckgo.com"] in
        search_and_print servers timeout words
    )
  |> Command.run
