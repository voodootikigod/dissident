{application, dissident,
 [{description, "Dissident Web Application Server"},
  {vsn, "0.01"},
  {modules, [
        dissident,
        dissident_app,
        dissident_skel,
        dissident_sup
	    ]},
  {registered, []},
  {mod, {mochiweb_app, []}},
  {env, []},
  {applications, [kernel, stdlib]}]}.
