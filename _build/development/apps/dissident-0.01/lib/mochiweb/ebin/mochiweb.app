%% This is the application resource file (.app file) for the mochiweb,
%% application.
{application, mochiweb, 
  [{description, "Your Desc HERE"},
   {vsn, "0.1.0"},
   {modules, [mochiweb_app,
              mochiweb_sup]},
   {registered,[mochiweb_sup]},
   {applications, [kernel, stdlib]},
   {mod, {mochiweb_app,[]}},
   {start_phases, []}]}.

