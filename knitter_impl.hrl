-record(error_callbacks, {ans_other = {knitter_error, ans_other, []}, % {Module, Function, User parameters}
			  tr_message_parse = {knitter_error, tr_message_parse, []},
			  tr_agent_info = {knitter_error, tr_agent_info, []},
			  tr_unable_connect = {knitter_error, tr_unable_connect, []},
			  tr_other = {knitter_error, tr_other, []},
			  unexpected_message = {knitter_error, unexpected_message, []},
			  start_protocol  = {knitter_error, start_protocol, []},
			  incomming_conversation = {knitter_error, incomming_conversation, []},
			  other = {knitter_error, other, []}
			 }).

-record(server_state, {name,
		       waiter_conv,
		       protocols = [],
		       active_protocols = [],
		       conversations = [],
		       expected_messages = [],
		       callbacks = #error_callbacks{}
		      }).
