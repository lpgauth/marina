% application
-define(APP, marina).
-define(CHILD(Mod), {Mod, {Mod, start_link, []}, permanent, 5000, worker, [Mod]}).
