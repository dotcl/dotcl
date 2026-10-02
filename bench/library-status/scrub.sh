# Sourced by run-quickload.sh and run-tests.sh (and check-scrub.sh): the
# filters a note goes through before it is written. Notes are published in
# docs/library-status.md, so anything in the error text that belongs to the
# measuring machine or to the build that measured it is replaced here.

# Error text carries whatever path the failure happened under: the measuring
# machine's home directory and checkout ("working directory
# 'C:Usersmeworkdotcl'", with the backslashes already gone). Replace any
# absolute path -- a drive letter followed by a path or a name, optionally
# after #P, or a /home /Users /tmp /mnt path, also when glued to a compiler
# flag such as -I/Users/... -- with <path>. A single letter before a colon is
# only taken as a drive when nothing alphanumeric precedes it, so package
# prefixes (ASDF/USER::X) are left alone.
scrub_paths() {
  sed -E 's@(^|[^A-Za-z0-9])(#P)?[A-Za-z]:[/A-Za-z][^ )"'"'"']*@\1<path>@g; s@(^|[^A-Za-z0-9]|-[A-Za-z]+)/(home|Users|tmp|mnt)/[^ )"'"'"']*@\1<path>@g'
}

# Libraries that refuse to run on an implementation they do not know often say
# which one, from LISP-IMPLEMENTATION-VERSION and MACHINE-TYPE: "not yet
# implemented for dotcl 0.1.29+273.ge515cc0 on Arm64". That names the build
# that happened to measure the row and the machine it ran on, neither of which
# is a fact about the library. Replace a development build's version (the
# +N.gHASH form) anywhere, a release version after "dotcl ", and the machine
# type after "dotcl <version> on ".
scrub_build() {
  sed -E 's@[0-9]+\.[0-9]+\.[0-9]+\+[0-9]+\.g[0-9a-f]+([.-]dirty)?@<version>@g; s@(dotcl )[0-9]+\.[0-9]+\.[0-9]+@\1<version>@g; s@(dotcl <version>) on [A-Za-z0-9_]+@\1 on <arch>@g'
}

scrub_note() {
  scrub_paths | scrub_build
}
