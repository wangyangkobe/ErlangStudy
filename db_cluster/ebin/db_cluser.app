{application,db_cluser,
             [{description,"The datebase cluster application."},
              {vsn,"1"},
              {registered,[dc_proxy,db_server,db_cluser_sup]},
              {applications,[kernel,stdlib]},
              {mod,{db_cluser_app,[]}},
              {env,[]},
              {modules,[client,db_cluser_app,db_cluser_sup,db_proxy,
                        db_server]}]}.
