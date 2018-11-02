(** This module launches processes isolated from the main environment
   using sandboxing technology. *)

type conf

val bare : conf
(** Configuration with all sharing disabled and an empty environment. *)

val conf : ?uid: int -> ?gid: int -> unit -> conf
(** Create a configuration with all sharing disabled, mounting in
   read-only mode /bin, /usr, /lib, /lib32 and /lib64 (if they exist)
   and on tmpfs /tmp, /run and /var.  The hostname is set to
   "OCaml". *)

val share_user : conf -> bool -> conf
val share_ipc : conf -> bool -> conf
val share_pid : conf -> bool -> conf
val share_net : conf -> bool -> conf
val share_uts : conf -> bool -> conf
val share_cgroup : conf -> bool -> conf

val uid : conf -> int -> conf
(** [uid c id] use a custom user [id] in the sandbox.  Automatically
   implies [share_user c false].  If [id < 0], unset it. *)

val gid : conf -> int -> conf
(** [gid c id] use a custom group [id] in the sandbox.  Automatically
   implies [share_user c false].  If [id < 0], unset it. *)

val hostname : conf -> string -> conf
(** [hostname c h] use the custom hostname [h] in the sandbox.
   Automatically implies [share_uts c false].  If [h = ""], unset it. *)

val setenv : conf -> string -> string -> conf
(** [setenv c var v] add the variable [var] with value [v] to the
   environment of the process. *)

val unsetenv : conf -> string -> conf
(** [unsetenv c var] remove the environment variable [var]. *)

(** {2 Filesystem related options} *)

val mount : conf -> ?dev:bool -> src:string -> ?rw:bool -> string -> conf
(** [mount c src dest] mount the host path [src] on [dest] in the
   sandbox.  The mounts are applied in the order they are set, the
   latter ones being able undo what the previous ones did.  Any
   missing parent directories that are required to create a specified
   destination are automatically created as needed.
   @param dev If [true], allow device access.
   @param rw  If [true], mount in read and write (default, mount read-only).

   Example: [let c = mount c "/a" "/a" in mount c ~rw:true "/a/b" "/a/b"] *)

val remount_ro : conf -> string -> conf
(** [remount_ro c dest] remount the path [dest] as readonly.  It works
   only on the specified mount point, without changing any other mount
   point under the specified path.  *)

val proc : conf -> string -> conf
(** [proc c dest] mount procfs on [dest].
   Example: [proc c "/proc"]. *)

val dev : conf -> string -> conf
(** [dev c dest] mount new devtmpfs on [dest].
   Example: [dev c "/dev"]. *)

val tmpfs : conf -> string -> conf
(** [tmpfs c dest] mount new tmpfs on [dest].
   Example: [tmpfs c "/var"] or [tmpfs c "/tmp"]. *)

val mqueue : conf -> string -> conf
(** [mqueue c dest] mount new mqueue on [dest]. *)

val dir : conf -> string -> conf
(** [dir c dest] create directory [dest] in the sandbox. *)

(* val file : conf -> Unix.file_descr -> string -> conf
 * val bind_data : conf -> Unix.file_descr -> ?ro: bool -> string -> conf *)

val symlink : conf -> src:string -> string -> conf
(** [symlink c src dest] create a symlink at [dest] with target [src]. *)

val new_session : conf -> bool -> conf
(** [new_session c b] when [b] is [true], create a new terminal
   session for the sandbox (calls setsid()).  This disconnects the
   sandbox from the controlling terminal which means the sandbox can't
   for instance inject input into the terminal.

   Note: In a general sandbox, if you don't use [new_session c true],
   it is recommended to use seccomp to disallow the TIOCSTI ioctl,
   otherwise the application can feed keyboard input to the terminal.
 *)


(** {2 Launch sandboxed processes} *)

val open_process_in : conf -> string -> string list -> in_channel
(** [open_process_in c cmd args] runs the command [cmd] with arguments
   [args] in a sandbox in parallel with the program.  The standard
   output of the program can be read on the returned channel. *)

val close_process_in : in_channel -> Unix.process_status


val open_process_out : conf -> string -> string list -> out_channel
(** [open_process_out c cmd args] runs the command [cmd] with
   arguments [args] in a sandbox in parallel with the program.  *)

val close_process_out : out_channel -> Unix.process_status


val open_process : conf -> string -> string list -> in_channel * out_channel
(** [open_process c cmd args] runs the command [cmd] with arguments
   [args] in a sandbox in parallel with the program.  *)

val close_process : in_channel * out_channel -> Unix.process_status


val open_process_full :
  conf -> string -> string list -> in_channel * out_channel * in_channel
(** [open_process_full c cmd args] runs the command [cmd] with
   arguments [args] in a sandbox in parallel with the program.  *)

val close_process_full :
  in_channel * out_channel * in_channel -> Unix.process_status
