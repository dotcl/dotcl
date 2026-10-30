# Sourced by run-quickload.sh and run-tests.sh: tell a failed network fetch
# (Quicklisp download, DNS) from a failure of the library.
#
#   net_failure LOG   succeeds when the log shows the fetch itself failing

net_failure() {
  grep -a -q -i -E 'Name service error|Temporary failure in name resolution|No such host is known|could not resolve host|Network is unreachable|Connection (refused|reset|timed out)|connect(ion)? timed out|HttpRequestException|SocketException' "$1"
}

NET_NOTE_PREFIX="network error: "
