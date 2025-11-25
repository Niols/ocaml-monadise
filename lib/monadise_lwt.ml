include Monadise.Make(struct
  type 'a t = 'a Lwt.t
  let return = Lwt.return
  let bind' ~on_error x f =
    Lwt.try_bind (fun () -> x) f (fun exn -> on_error (); Lwt.fail exn)
end)
