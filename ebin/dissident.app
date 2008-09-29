{application, dissident,
 [{description, "dissident"},
  {vsn, "0.01"},
  {modules, [
    dissident,
    dissident_app,
    dissident_sup,
    dissident_web,
    dissident_deps
  ]},
  {registered, []},
  {mod, {dissident_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
