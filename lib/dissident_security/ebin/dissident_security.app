%% This is the application resource file (.app file) for the dissident,
%% application.
{application, dissident_security, 
  [{description, "Dissident Security Module."},
   {vsn, "0.1.0"},
   {modules, [dissident_security,
              dissident_security_app,
              dissident_security_sup]},
   {registered,[dissident_security_sup]},
   {applications, [kernel, stdlib]},
   {mod, {dissident_security_app,[]}},
   {start_phases, []}]}.