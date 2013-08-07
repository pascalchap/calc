{application,calc,
             [{description,"a calculator"},
              {vsn,"0.2.0"},
              {modules,[calc,calc_app,calc_server,calc_store,calc_sup,
                        calcgui]},
              {registered,[calc_sup]},
              {applications,[kernel,stdlib]},
              {mod,{calc_app,[]}},
              {env,[]}]}.
