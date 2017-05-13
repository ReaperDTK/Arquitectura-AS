OUT=./bin
SSL_CERT=-ssl_dist_opt server_certfile "ssl/cert.pem" -ssl_dist_opt server_keyfile "ssl/key.pem"
DEFAULT_SRV=server@127.0.0.1
DEFAULT_CL= client@127.0.0.1

chat:
	erlc -o $(OUT) ./src/*/*.erl

clean:
	rm ./bin/*.beam

run:
	erl  -pz $(OUT) -name $(name)

run_ssl_server:
	erl   -pz $(OUT) -s ssl start -proto_dist inet_tls $(SSL_CERT) -name $(name)

run_ssl_client:
	erl   -pz $(OUT) -s ssl start -proto_dist inet_tls -name $(name)
