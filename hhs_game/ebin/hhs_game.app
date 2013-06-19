{application,hhs_game,
             [{description,"The HHS game server."},
              {vsn,"1"},
              {modules,[client,connect_listen,hall,hhs_game_app,hhs_game_sup,
                        room]},
              {registered,[hall,room]},
              {applications,[kernel,stdlib]},
              {mod,{hhs_game_app,[]}},
              {env,[]}]}.
