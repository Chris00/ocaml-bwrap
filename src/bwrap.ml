
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
end

type fs =
  | Bind of { src: string;  dest: string }
  | Bind_ro of { src: string;  dest: string }
  | Bind_dev of { src: string;  dest: string }
  | Remount_ro of string
  | Tmpfs of string
  | Mqueue of string
  | Dir of string
  | Symlink of { src: string;  dest: string }

type conf = {
    unshare_user: bool;
    unshare_ipc: bool;
    unshare_pid: bool;
    unshare_net: bool;
    unshare_uts: bool;
    unshare_cgroup: bool;
    uid: int;  (* < 0 if unset *)
    gid: int;  (* < 0 if unset *)
    hostname: string;  (* = "" if unset *)
    env: string SMap.t;  (* variable → content *)
    fs: fs list;  (* first = last entered *)
    proc: string;  (* = "" if not set *)
    dev: string;   (* = "" if not set *)
  }

let conf = {
    unshare_user = true;
    unshare_ipc = true;
    unshare_pid = true;
    unshare_net = true;
    unshare_uts = true;
    unshare_cgroup = true;
    uid = -1;
    gid = -1;
    hostname = "";
    env = SMap.empty;
    fs = [];
    proc = "";
    dev = "";
  }

let share_user c b = {c with unshare_user = not b}
let share_ipc c b = {c with unshare_ipc = not b}
let share_pid c b = {c with unshare_pid = not b}
let share_net c b = {c with unshare_net = not b}
let share_uts c b = {c with unshare_uts = not b}
let share_cgroup c b = {c with unshare_cgroup = not b}

let uid c uid =
  if uid < 0 then {c with uid}
  else {c with uid;  unshare_user = true}

let gid c gid =
  if gid < 0 then {c with gid}
  else {c with gid;  unshare_user = true}

let hostname c h =
  if h = "" then {c with hostname = h}
  else {c with hostname = h;  unshare_uts = true}

let setenv c var v = {c with env = SMap.add var v c.env}

let unsetenv c var = {c with env = SMap.remove var c.env}

let mount c ?(dev=false) ~src ?(rw=false) dest =
  let b = if dev then Bind_dev {src; dest}
           else if rw then Bind {src; dest}
           else Bind_ro {src; dest} in
  {c with fs = b :: c.fs}

let remount_ro c dest =
  {c with fs = Remount_ro dest :: c.fs}

let proc c dest = {c with proc = dest}
let dev c dest = {c with dev = dest}

let tmpfs c dest = {c with fs = Tmpfs dest :: c.fs}

let mqueue c dest = {c with fs = Mqueue dest :: c.fs}

let dir c dest = {c with fs = Dir dest :: c.fs}

let symlink c ~src dest = {c with fs = Symlink {src; dest} :: c.fs}

(** Add a command line option with 1 argument [v].  It is assumed that
   [o] starts and ends with a space. *)
let[@inline] add_arg1 a o v =
  Buffer.add_string a o;
  Buffer.add_string a (Filename.quote v)

(** Add a command line option with 2 arguments [v1] and [v2]. It is
   assumed that [o] starts and ends with a space. *)
let[@inline] add_arg2 a o v1 v2 =
  Buffer.add_string a o;
  Buffer.add_string a (Filename.quote v1);
  Buffer.add_char a ' ';
  Buffer.add_string a (Filename.quote v2)

let[@inline] add_bind a m = match m with
  | Bind b -> add_arg2 a " --bind " b.src b.dest
  | Bind_ro b -> add_arg2 a " --ro-bind " b.src b.dest
  | Bind_dev b -> add_arg2 a " --dev-bind " b.src b.dest
  | Remount_ro dest -> add_arg1 a " --remount-ro " dest
  | Tmpfs dest -> add_arg1 a " --tmpfs " dest
  | Mqueue dest -> add_arg1 a " --mqueue " dest
  | Dir dest -> add_arg1 a " --dir " dest
  | Symlink b -> add_arg2 a " --symlink " b.src b.dest

let make_cmd c ~env cmd args =
  let a = Buffer.create 1024 in
  Buffer.add_string a "bwrap";
  if c.unshare_user then Buffer.add_string a " --unshare-user";
  if c.unshare_ipc then Buffer.add_string a " --unshare-ipc";
  if c.unshare_pid then Buffer.add_string a " --unshare-pid";
  if c.unshare_net then Buffer.add_string a " --unshare-net";
  if c.unshare_uts then Buffer.add_string a " --unshare-uts";
  if c.unshare_cgroup then Buffer.add_string a  " --unshare-cgroup";
  if c.uid >= 0 then (Buffer.add_string a " --uid ";
                     Buffer.add_string a (string_of_int c.uid));
  if c.gid >= 0 then (Buffer.add_string a " --gid ";
                     Buffer.add_string a (string_of_int c.gid));
  if c.hostname <> "" then add_arg1 a " --hostname " c.hostname;
  (* Beware that the order of binds is important. *)
  if c.proc <> "" then add_arg1 a " --proc " c.proc;
  if c.dev <> "" then add_arg1 a " --dev " c.dev;
  List.iter (add_bind a) (List.rev c.fs);
  if env then (
    (* Unset all variables of the environment and then set the ones
       declared in [c]. *)
    let e = Unix.environment () in
    Array.iter (fun e -> match String.split_on_char '=' e with
                         | v :: _ -> add_arg1 a " --unsetenv " v
                         | [] -> assert false (* See split_on_char *)) e;
    SMap.iter (fun var v -> add_arg2 a " --setenv " var v) c.env;
  );
  (* Command to run. *)
  Buffer.add_char a ' ';
  Buffer.add_string a (Filename.quote cmd);
  List.iter (fun x -> Buffer.add_char a ' ';
                      Buffer.add_string a (Filename.quote x)) args;
  Buffer.contents a


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
