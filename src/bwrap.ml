
module SMap = Map.Make(String)

module String = struct
  include String

  (* To be compatible with OCaml < 4.04.0 *)
  let split_on_char sep s =
    let r = ref [] in
    let j = ref (length s) in
    for i = length s - 1 downto 0 do
      if unsafe_get s i = sep then begin
          r := sub s (i + 1) (!j - i - 1) :: !r;
          j := i
        end
    done;
    sub s 0 !j :: !r

  let rec sum_lengths acc seplen = function
    | [] -> acc
    | [hd] -> String.length hd + acc
    | hd :: tl -> sum_lengths (String.length hd + seplen + acc) seplen tl

  let rec unsafe_rev_blits dst pos sep seplen = function
    | [] -> dst
    | [hd] -> unsafe_blit hd 0 dst 0 (String.length hd);  dst
    | hd :: tl -> let pos = pos - String.length hd in
                  unsafe_blit hd 0 dst pos (String.length hd);
                  let pos = pos - seplen in
                  unsafe_blit sep 0 dst pos seplen;
                  unsafe_rev_blits dst pos sep seplen tl

  (* Equivalent to [String.concat sep (List.rev l)]. *)
  let rev_concat sep = function
    | [] -> ""
    | l -> let seplen = String.length sep in
           let len = sum_lengths 0 seplen l in
           Bytes.unsafe_to_string
             (unsafe_rev_blits (Bytes.create len) len sep seplen l)
end

type conf = {
    args: string list; (* In reverse order *)
    unshare_user: bool;
    unshare_user_mandatory: bool;
    unshare_ipc: bool;
    unshare_pid: bool;
    unshare_net: bool;
    unshare_uts: bool;
    unshare_uts_mandatory: bool;
    unshare_cgroup: bool;
    uid: int;  (* < 0 if unset *)
    gid: int;  (* < 0 if unset *)
    hostname: string;  (* = "" if unset *)
    env: string SMap.t;  (* variable → content *)
    proc: string;  (* = "" if not set *)
    dev: string;   (* = "" if not set *)
    new_session: bool;
    die_with_parent: bool;
  }

let[@inline] add_arg1 o v1 c = { c with args = v1 :: o :: c.args}
let[@inline] add_arg2 o v1 v2 c = { c with args = v2 :: v1 :: o :: c.args}

let bare = {
    args = ["bwrap"];
    unshare_user = true;
    unshare_user_mandatory = false;
    unshare_ipc = true;
    unshare_pid = true;
    unshare_net = true;
    unshare_uts = true;
    unshare_uts_mandatory = false;
    unshare_cgroup = true;
    uid = -1;
    gid = -1;
    hostname = "";
    env = SMap.empty;
    proc = "";
    dev = "";
    new_session = true;
    die_with_parent = true;
  }

let conf ?uid ?gid () =
  let uid = match uid with Some u -> u | None -> -1 in
  let gid = match gid with Some u -> u | None -> -1 in
  { (* Beware [args] is in reverse order. *)
    args = ["/tmp"; "--tmpfs";
            "/run"; "--tmpfs";
            "/var"; "--tmpfs";
            "/lib64"; "/lib64"; "--ro-bind-try";
            "/lib32"; "/lib32"; "--ro-bind-try";
            "/lib"; "/lib"; "--ro-bind-try";
            "/usr"; "/usr"; "--ro-bind-try";
            "/bin"; "/bin"; "--ro-bind-try";
            "bwrap"];
    unshare_user = true;
    unshare_user_mandatory = false;
    unshare_ipc = true;
    unshare_pid = true;
    unshare_net = true;
    unshare_uts = true;
    unshare_uts_mandatory = false;
    unshare_cgroup = true;
    uid;
    gid;
    hostname = "OCaml";
    env = SMap.empty;
    proc = "/proc";
    dev = "/dev";
    new_session = true;
    die_with_parent = true;
  }

let share_user b c = {c with unshare_user = not b}
let share_ipc b c = {c with unshare_ipc = not b}
let share_pid b c = {c with unshare_pid = not b}
let share_net b c = {c with unshare_net = not b}
let share_uts b c = {c with unshare_uts = not b}
let share_cgroup b c = {c with unshare_cgroup = not b}

let uid uid c =
  if uid < 0 then {c with uid}
  else {c with uid;  unshare_user_mandatory = true}

let gid gid c =
  if gid < 0 then {c with gid}
  else {c with gid;  unshare_user_mandatory = true}

let hostname h c =
  if h = "" then {c with hostname = ""}
  else {c with hostname = Filename.quote h;  unshare_uts_mandatory = true}

let setenv var v c = {c with env = SMap.add var v c.env}

let unsetenv var c = {c with env = SMap.remove var c.env}

let mount ?(dev=false) ?src ?(rw=false) dest c =
  let dest = Filename.quote dest in
  let src = match src with Some s -> Filename.quote s
                         | None -> dest in
  if dev then add_arg2 "--dev-bind" src dest c
  else if rw then add_arg2 "--bind" src dest c
  else add_arg2 "--ro-bind" src dest c

let remount_ro dest c =
  add_arg1 "--remount-ro" (Filename.quote dest) c

let proc dest c = {c with proc = Filename.quote dest}
let dev dest c = {c with dev = Filename.quote dest}

let tmpfs dest c = add_arg1 "--tmpfs" (Filename.quote dest) c

let mqueue dest c = add_arg1 "--mqueue" (Filename.quote dest) c

let dir dest c = add_arg1 "--dir" (Filename.quote dest) c

let symlink ?src dest c =
  let dest = Filename.quote dest in
  let src = match src with Some s -> Filename.quote s
                         | None -> dest in
  add_arg2 "--symlink" src dest c

let chdir dir c = add_arg1 "--chdir" (Filename.quote dir) c

let new_session b c = {c with new_session = b}

let die_with_parent b c = {c with die_with_parent = b}


let make_cmd c ~env cmd args =
  let a = if c.unshare_user_mandatory || c.unshare_user then
            "--unshare-user" :: c.args
          else c.args in
  let a = if c.unshare_ipc then "--unshare-ipc" :: a else a in
  let a = if c.unshare_pid then "--unshare-pid" :: a else a in
  let a = if c.unshare_net then "--unshare-net" :: a else a in
  let a = if c.unshare_uts_mandatory || c.unshare_uts then
            "--unshare-uts" :: a
          else a in
  let a = if c.unshare_cgroup then "--unshare-cgroup" :: a else a in
  let a = if c.uid >= 0 then string_of_int c.uid :: "--uid" :: a else a in
  let a = if c.gid >= 0 then string_of_int c.gid :: "--gid" :: a else a in
  let a = if c.hostname <> "" then c.hostname :: "--hostname" :: a else a in
  let a = if c.proc <> "" then c.proc :: "--proc" :: a else a in
  let a = if c.dev <> "" then c.dev :: "--dev" :: a else a in
  let a = if c.new_session then "--new-session" :: a else a in
  let a = if c.die_with_parent then "--die-with-parent" :: a else a in
  let a =
    if env then (
      (* Unset all variables of the environment and then set the ones
         declared in [c]. *)
      let e = Unix.environment () in
      let a = Array.fold_left
                (fun a e -> match String.split_on_char '=' e with
                            | v :: _ -> v :: "--unsetenv" :: a
                            | [] -> assert false (* See split_on_char *)
                ) a e in
      SMap.fold (fun var v a -> v :: var :: "--setenv" :: a) c.env a
    )
    else a in
  (* Command to run. *)
  let a = Filename.quote cmd :: a in
  let a = List.fold_left (fun a x -> Filename.quote x :: a) a args in
  String.rev_concat " " a


let open_process_in c cmd args =
  Unix.open_process_in (make_cmd c cmd args ~env:true)

let close_process_in = Unix.close_process_in

let open_process_out c cmd args =
  Unix.open_process_out (make_cmd c cmd args ~env:true)

let close_process_out = Unix.close_process_out

let open_process c cmd args =
  Unix.open_process (make_cmd c cmd args ~env:true)

let close_process = Unix.close_process

let make_env c =
  Array.of_list(SMap.fold (fun var v l -> (var ^ "=" ^ v) :: l) c.env [])

let open_process_full c cmd args =
  Unix.open_process_full (make_cmd c cmd args ~env:false) (make_env c)

let close_process_full = Unix.close_process_full
