-module(features_handler_v0).

-export([error_schema/0,
         not_found_spec/0]).

not_found_spec() ->
    #{
        description => <<"Not found on this server">>,
        content => #{
            'application/json' => #{
                schema => error_schema()
        }}
    }.

error_schema() ->
    #{
        type => object,
        properties => #{
           <<"error">> => #{
              type => object,
              description => <<"Object describing the error">>
           }
        }
    }.
