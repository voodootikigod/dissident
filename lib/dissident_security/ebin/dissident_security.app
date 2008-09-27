%% This is the application resource file (.app file) for the dissident,
%% application.
{application, dissident_security, 
  [{description, "Dissident Security Module."},
   {vsn, "0.1.0"},
   {modules, []},
   {registered,[dissident_security_sup]},
   {applications, [kernel, stdlib]},
   {mod, {dissident_security_app,[]}},
   {start_phases, []}]}.