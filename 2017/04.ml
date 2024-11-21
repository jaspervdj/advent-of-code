module StringSet = Set.Make(String);;

let is_valid_passphrase passphrase =
    let rec go acc words0 =
        match words0 with
        | [] -> true
        | w :: words1 ->
            if StringSet.mem w acc then false
            else go (StringSet.add w acc) words1 in
    go StringSet.empty (String.split_on_char ' ' passphrase);;

let count_valid_passphrases channel =
    let rec go acc0 =
        try
            let passphrase = input_line channel in
            let acc1 =
                if is_valid_passphrase passphrase then acc0 + 1
                else acc0 in
            go acc1
        with
            End_of_file -> acc0 in
    go 0;;

print_int (count_valid_passphrases stdin);
print_string "\n";;
