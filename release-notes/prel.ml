open Containers
open Printf
open Astring
open Git_unix
module H = Store.Hash
module Commit = Git.Commit.Make (H)
module Seq = OSeq
open Lwt.Infix
open Fun

let log x = eprintf x

let ddb x = eprintf x

module Traverse = struct
  type 'commit commit =
    | Single of 'commit
    | Merge of 'commit
  [@@deriving map]

  let cons_commit = function
    | Merge _ -> fun c -> Merge c
    | Single _ -> fun c -> Single c

  let read_commit store commit =
    match%lwt Store.(read_exn store commit) with
    | Commit commit -> Lwt.return_some commit
    | _ -> Lwt.return_none

  let rec traverse store last_commit commit acc =
    if H.equal commit last_commit then Lwt.return acc
    else (
      match%lwt read_commit store commit with
      | None -> Lwt.return acc
      | Some c ->
        ( match Commit.parents c with
          | [] -> Lwt.return acc
          | [ next ] -> traverse store last_commit next (Single commit :: acc)
          | next :: _merge -> traverse store last_commit next (Merge commit :: acc) ) )

  let git_log ~from_commit ~to_commit store =
    traverse store to_commit from_commit []
    >>= Lwt_list.filter_map_s (function (Single commit | Merge commit) as c ->
        read_commit store commit >|= Option.map (cons_commit c) )
    >|= List.rev
    >|= Seq.of_list

  let resolve_ref store reference =
    match%lwt Store.(Ref.resolve store reference) with
    | Ok commit -> Lwt.return_some commit
    | Error `Not_found -> Lwt.return_none
    | Error e ->
      Lwt.fail
        (Failure
           (sprintf "failed to read reference %s : %s"
              (Store.Reference.to_string reference)
              (Format.to_string Store.pp_error e)))

  let log_ref store up_to_ref =
    let%lwt head = resolve_ref store Store.(Reference.head) >|= Option.get_exn in
    match%lwt resolve_ref store (Store.Reference.of_string up_to_ref) with
    | None -> Lwt.return_none
    | Some commit ->
      ddb "resolving log from %s to %s\n%!" (H.to_hex commit) (H.to_hex head);
      git_log ~from_commit:head ~to_commit:commit store >|= Option.pure
end

module Parse = struct
  type change = {
    pr_number : int option;
    title : string;
    commits : string list;
    author : string;
    commit : string
  }

  let merge_header = Re2.create_exn ~options:[ `Perl_classes true ] {|Merge pull request #(\d+)|}

  let squash_header = Re2.create_exn ~options:[ `Perl_classes true ] {|\(#(\d+)\)|}

  let extract_merge_pr message =
    match Re2.find_submatches merge_header message with
    | Ok [| _; Some id |] -> Option.wrap int_of_string id
    | _ -> None

  let make_merge_pr ~author commit message =
    match extract_merge_pr message with
    | None -> { pr_number = None; commits = []; title = message; author; commit }
    | pr_number ->
      let title = String.drop ~sat:(not % Char.equal '\n') message |> String.trim in
      { pr_number; commits = []; title; author; commit }

  let make_squash_pr ~author commit message =
    match String.cuts ~empty:false ~sep:"\n" message with
    | [] -> { pr_number = None; commits = []; title = message; author; commit }
    | header :: commits ->
      let title = Re2.replace_exn ~f:(fun _ -> "") squash_header header |> String.trim in
      let commits = List.map String.trim commits |> List.filter (not % String.is_empty) in
      let pr_number =
        Re2.find_first ~sub:(`Index 0) squash_header header
        |> Result.map (Option.wrap int_of_string)
        |> Result.get_or ~default:None
      in
      { pr_number; commits; title; author; commit }

  let make_merge ~author commit message =
    let title = String.cut ~sep:"\n" message |> Option.map fst |> Option.get_or ~default:message in
    { author; title; pr_number = None; commits = []; commit }

  let commit_message = String.trim % Commit.message

  let ghe_email = "XXX"

  let is_ghe commit = String.equal (Commit.committer commit).Git.User.email ghe_email

  let looks_like_squash_merge message =
    match String.cuts ~empty:false ~sep:"\n" message with
    | [] -> false
    | header :: _ -> Re2.matches squash_header header

  let looks_like_merge message =
    match String.cuts ~empty:false ~sep:"\n" message with
    | [] -> false
    | header :: _ -> Re2.matches merge_header header

  let make_merge commit =
    let message = commit_message commit in
    let author = (Commit.author commit).name in
    let commit_hex = Commit.digest commit |> H.to_hex in
    if is_ghe commit || looks_like_merge message then make_merge_pr ~author commit_hex message
    else make_merge ~author commit_hex message

  let make_commit commit =
    let open Traverse in
    let message = commit_message commit in
    let author = (Commit.author commit).name in
    let commit_hex = Commit.digest commit |> H.to_hex in
    if is_ghe commit || looks_like_squash_merge message then
      Merge (make_squash_pr ~author commit_hex message)
    else Single { author; title = message; pr_number = None; commits = []; commit = commit_hex }

  let history =
    let open Traverse in
    Seq.map (function
        | Single commit -> make_commit commit
        | Merge commit -> Merge (make_merge commit) )
end

module Render_html : sig
  val make : title:string -> repo:string -> Parse.change Traverse.commit Seq.t -> unit
end = struct
  open Tyxml
  open Html

  let style = {|
  body {
    color: #444d56;
    line-height: 1.5;
    margin-left: 5em
    font-family: Helvetica, Sans;
    font-size: 12px;
  }
  a {
    text-decoration: none;
  }
  li {
    list-style-type: none;
  }
  .pr_number {
    color: #28a745;
  }
  .commit_number {
    color: #0366d6;
  }
  .pr_number, .commit_number {
    width: 6em;
  }
  .author {
    margin-left: 1em
  }
  .title {
    font-size: 16px;
    font-weight: 600;
    margin-top: 2ex;
    display: flex;
  }
  .subject {
    margin-left: 2em
  }
  .author {
    font-weight: 100;
  }
  .commits {
    margin-left: 12em;
    margin-top: 1ex
  }
  |}

  let base_ghe_url = "https://example.com"

  let make_link ~kind ~repo target = sprintf "%s/%s/%s/%s" base_ghe_url repo kind target

  let make_pr_link ~repo pr_number = make_link ~kind:"pull" ~repo (string_of_int pr_number)

  let make_commit_link ~repo commit = make_link ~kind:"commit" ~repo commit

  let make_page ~title body =
    [%html "<html><head><title>" (txt title) "</title><style>" [cdata_style style] "</style></head><body>" body "</body></html>"]
    |> Format.printf "%a" (pp ())

  let make_commits_list commits =
    let commits = List.map (fun commit -> [%html "<li>" [ txt commit ] "</li>"]) commits in
    [%html "<ul>" commits "</ul>"]

  let pr_number ~repo { Parse.pr_number; commit; _ } =
    match pr_number with
    | None ->
      [%html
        "<a class='commit_number' href=" (make_commit_link ~repo commit) ">"
          [ txt (String.take ~max:8 commit) ]
          "</a>"]
    | Some pr_number ->
      [%html
        "<a class='pr_number' href=" (make_pr_link ~repo pr_number) ">"
          [ txt ( "#" ^ string_of_int pr_number) ]
          "</a>"]

  let make_merge ~repo ({ Parse.title; commits; author; _ } as change) =
    [%html
      "<li class='merge'>"
        "<div class='title'>" [pr_number ~repo change]"<div class='subject'>" [txt title] "</div><div class='author'>" [txt author] "</div></div>"
        "<div class='commits'>"[make_commits_list commits] "</div>"
        "</li>"
    ][@@ocamlformat "disable=true"]

  let make_commit ~repo ({ Parse.title; author; _ } as change) =
    [%html
      "<li class='commit'>"
        "<div class='title'>"[ pr_number ~repo change] "<div class='subject'>" [txt title]"</div><div class='author'>" [txt author] "</div></div>"
        "</li>"
    ][@@ocamlformat "disable=true"]

  let make_entry ~repo =
    let open Traverse in
    function
    | Single commit -> make_commit ~repo commit
    | Merge commit -> make_merge ~repo commit

  let make ~title ~repo prs =
    let prs = prs |> Seq.map (make_entry ~repo) |> Seq.to_list in
    make_page ~title [ [%html "<ul>" prs "</ul>"] ]
end

let run_and_print repo root last_commit =
  let%lwt store = Store.v root >|= Result.get_exn in
  match%lwt Traverse.log_ref store last_commit with
  | None ->
    log "no such commit %s\n" last_commit;
    Lwt.return_unit
  | Some log ->
    log |> Parse.history |> Render_html.make ~title:"Release log" ~repo;
    Lwt.return_unit

let main () =
  let last_commit = "refs/tags/" ^ Sys.argv.(1) in
  let root = Sys.getcwd () in
  let repo = Filename.(basename root) in
  log "reading repo %s (%s)\n" root repo;
  let root = Fpath.(v root) in
  Lwt_main.run @@ run_and_print repo root last_commit

let _ = main ()
