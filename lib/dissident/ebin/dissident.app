%% This is the application resource file (.app file) for the dissident,
%% application.
{application, dissident, 
  [{description, "Dissident Web Application Framework that puts Javascript and Erlang together."},
   {vsn, "0.1.0"},
   {modules, [dissident,
              dissident_app,
              dissident_sup,
              dissident_web,
              dissident_deps]},
   {registered,[dissident_sup]},
   {applications, [kernel, stdlib, mochiweb]},
   {mod, {dissident_app,[]}},
   {start_phases, []}]}.