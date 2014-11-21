Go package: util, dag

export GOPATH={path to idea projects}/misc/src/go

To verify compilation
  cd {util|dag}
  go build

To run tests
  go test

To compile the tests for debugging with gdb
  go test -c -gcflags "-N -l"

To view the godoc generated document