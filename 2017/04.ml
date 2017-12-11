module StringSet = Set.Make(String);;

let is_valid_passphrase passphrase =
    let rec go acc words0 =
        match words0 with
        | [] -> true
        | w :: words1 ->
            if StringSet.mem w acc then false
            else go (StringSet.add w acc) words1 in
    go StringSet.empty (String.split_on_char ' ' passphrase);;

let count_valid_passphrases stream =
    let rec go acc0 =
        match Stream.peek stream with
        | None -> acc0
        | Some passphrase ->
            Stream.junk stream;
            let acc1 =
                if is_valid_passphrase passphrase then acc0 + 1
                else acc0 in
            go acc1 in
    go 0;;

let line_stream_of_channel channel = Stream.from
      (fun _ -> try Some (input_line channel) with End_of_file -> None);;

print_int (count_valid_passphrases (line_stream_of_channel stdin));;
print_string "\n";;
