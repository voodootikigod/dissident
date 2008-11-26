{application, dissident,
 [{description, "dissident"},
  {vsn, "0.1"},
  {modules, [
	     dissident,
	     dissident_app,
	     dissident_sup,
	     dissident_resource
	    ]},
  {registered, []},
  {mod, {dissident_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
