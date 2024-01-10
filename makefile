default: lib/generated/idl/anoma.pb.ex

lib/generated/idl/anoma.pb.ex: idl/anoma.proto
	protoc --elixir_out=plugins=grpc:lib/generated/ idl/anoma.proto

clean:
	rm -rf lib/generated/idl/anoma.pb.ex
